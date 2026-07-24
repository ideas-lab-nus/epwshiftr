#!/usr/bin/env Rscript

# Resolve this script even when it is launched outside the package root.
daily_probe__script_path <- function() {
    file_arg <- grep(
        "^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    if (!length(file_arg)) {
        return(NULL)
    }
    normalizePath(
        sub("^--file=", "", file_arg[[1L]]),
        winslash = "/",
        mustWork = TRUE
    )
}

# Locate the source checkout or installed package beside the implementation.
daily_probe__package_root <- function(script_path = daily_probe__script_path()) {
    if (is.null(script_path)) {
        candidates <- normalizePath(
            getwd(), winslash = "/", mustWork = TRUE)
    } else {
        # The implementation lives in installed `tools/` and source
        # `inst/tools/`; the repository wrapper lives in source `tools/`.
        candidates <- unique(vapply(
            c(
                file.path(dirname(script_path), ".."),
                file.path(dirname(script_path), "..", "..")
            ),
            normalizePath,
            character(1L),
            winslash = "/",
            mustWork = TRUE
        ))
    }
    for (candidate in candidates) {
        description <- file.path(candidate, "DESCRIPTION")
        if (file.exists(description) &&
            any(grepl(
                "^Package:\\s*epwshiftr\\s*$",
                readLines(description, warn = FALSE)
            ))) {
            return(candidate)
        }
    }
    stop(sprintf(
        "Cannot locate epwshiftr from: %s",
        paste(candidates, collapse = ", ")
    ),
        call. = FALSE
    )
}

# Print a compact command reference without loading the package or using ESGF.
daily_probe__usage <- function() {
    paste(
        "Usage:",
        "  Rscript tools/probe-daily-cmip6-availability.R [options]",
        "",
        "Options:",
        "  --models=auto|ID,...         all discovered or selected source IDs (default: auto)",
        "  --scenarios=ID,...           future experiments (default: ssp245,ssp585)",
        "  --member=LABEL|auto           realization (default: r1i1p1f1)",
        "  --future-years=START:END      future coverage window (default: 2041:2070)",
        "  --historical-years=START:END  historical window (default: 1995:2014)",
        "  --index-nodes=NAME|URL,...    ordered ESGF nodes (default: DKRZ,CEDA,ORNL)",
        "  --max-nodes=N                 maximum index nodes surveyed (default: 3)",
        "  --query-timeout=SECONDS       one ESGF response timeout (default: 45)",
        "  --connect-timeout=SECONDS     one connection timeout (default: 10)",
        "  --data-node=HOST              optional ESGF data-node filter",
        "  --output=PATH                 receipt directory",
        "  --ui=log|none                 query progress output (default: log)",
        "  --plan                        print resolved configuration only",
        "  -h, --help                    show this help",
        sep = "\n"
    )
}

# Parse one comma-separated option into unique, non-empty values.
daily_probe__csv_values <- function(value, option) {
    values <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
    values <- unique(values[nzchar(values)])
    if (!length(values)) {
        stop(sprintf("`%s` must contain at least one value.", option),
            call. = FALSE)
    }
    values
}

# Parse an inclusive year range or an explicit comma-separated year vector.
daily_probe__years <- function(value, option) {
    if (grepl("^[0-9]{4}:[0-9]{4}$", value)) {
        bounds <- as.integer(strsplit(value, ":", fixed = TRUE)[[1L]])
        if (bounds[[1L]] > bounds[[2L]]) {
            stop(sprintf(
                "`%s` range start must not be after its end.", option),
                call. = FALSE)
        }
        years <- seq.int(bounds[[1L]], bounds[[2L]])
    } else {
        years <- suppressWarnings(as.integer(
            daily_probe__csv_values(value, option)))
    }
    if (!length(years) || anyNA(years) ||
        any(years < 1850L | years > 2300L)) {
        stop(sprintf("`%s` must contain valid four-digit years.", option),
            call. = FALSE)
    }
    sort(unique(years))
}

# Convert command-line arguments into one validated, testable probe config.
daily_probe__parse_args <- function(args, now = Sys.time()) {
    timestamp <- format(now, "%Y%m%d-%H%M%S", tz = "UTC")
    config <- list(
        models = NULL,
        scenarios = c("ssp245", "ssp585"),
        member = "r1i1p1f1",
        future_years = 2041:2070,
        historical_years = 1995:2014,
        index_nodes = c("DKRZ", "CEDA", "ORNL"),
        max_nodes = 3L,
        query_timeout = 45,
        connect_timeout = 10,
        data_node = NULL,
        output = file.path(
            "outputs", paste0("daily-cmip6-availability-", timestamp)),
        ui = "log",
        plan = FALSE,
        help = FALSE
    )

    for (arg in args) {
        if (arg %in% c("-h", "--help")) {
            config$help <- TRUE
            next
        }
        if (identical(arg, "--plan")) {
            config$plan <- TRUE
            next
        }
        if (!startsWith(arg, "--") || !grepl("=", arg, fixed = TRUE)) {
            stop(sprintf("Unsupported argument: %s", arg), call. = FALSE)
        }
        separator <- regexpr("=", arg, fixed = TRUE)[[1L]]
        name <- substring(arg, 3L, separator - 1L)
        value <- substring(arg, separator + 1L)
        if (identical(name, "models")) {
            config$models <- if (identical(value, "auto")) {
                NULL
            } else {
                daily_probe__csv_values(value, "--models")
            }
        } else if (identical(name, "scenarios")) {
            config$scenarios <- daily_probe__csv_values(
                value, "--scenarios")
        } else if (identical(name, "member")) {
            if (!nzchar(value)) {
                stop("`--member` cannot be empty.", call. = FALSE)
            }
            config$member <- if (identical(value, "auto")) NULL else value
        } else if (identical(name, "future-years")) {
            config$future_years <- daily_probe__years(
                value, "--future-years")
        } else if (identical(name, "historical-years")) {
            config$historical_years <- daily_probe__years(
                value, "--historical-years")
        } else if (identical(name, "index-nodes")) {
            config$index_nodes <- daily_probe__csv_values(
                value, "--index-nodes")
        } else if (identical(name, "max-nodes")) {
            config$max_nodes <- suppressWarnings(as.integer(value))
        } else if (identical(name, "query-timeout")) {
            config$query_timeout <- suppressWarnings(as.numeric(value))
        } else if (identical(name, "connect-timeout")) {
            config$connect_timeout <- suppressWarnings(as.numeric(value))
        } else if (identical(name, "data-node")) {
            config$data_node <- if (nzchar(value)) value else NULL
        } else if (identical(name, "output")) {
            if (!nzchar(value)) {
                stop("`--output` cannot be empty.", call. = FALSE)
            }
            config$output <- value
        } else if (identical(name, "ui")) {
            config$ui <- match.arg(value, c("log", "none"))
        } else {
            stop(sprintf("Unknown option: --%s", name), call. = FALSE)
        }
    }
    if (is.na(config$max_nodes) || config$max_nodes < 1L) {
        stop("`--max-nodes` must be a positive integer.", call. = FALSE)
    }
    if (is.na(config$query_timeout) || !is.finite(config$query_timeout) ||
        config$query_timeout < 1) {
        stop("`--query-timeout` must be at least one second.",
            call. = FALSE)
    }
    if (is.na(config$connect_timeout) ||
        !is.finite(config$connect_timeout) ||
        config$connect_timeout < 1) {
        stop("`--connect-timeout` must be at least one second.",
            call. = FALSE)
    }
    config
}

# Load an adjacent source checkout when available, otherwise reuse the
# installed package namespace that supplied this implementation.
daily_probe__load_package <- function(package_root) {
    if (file.exists(file.path(package_root, "R", "daily-cmip6.R"))) {
        if (!requireNamespace("pkgload", quietly = TRUE)) {
            stop("The source-checkout probe requires the pkgload package.",
                call. = FALSE)
        }
        pkgload::load_all(package_root, quiet = TRUE)
    } else {
        loadNamespace("epwshiftr")
    }
    invisible(TRUE)
}

# Accept stable node labels as well as explicit URLs and normalize aliases.
daily_probe__resolve_nodes <- function(values, max_nodes) {
    index_nodes <- getFromNamespace("INDEX_NODES", "epwshiftr")
    normalize_node <- getFromNamespace("query__normalize_node", "epwshiftr")
    nodes <- vapply(values, function(value) {
        if (value %in% names(index_nodes)) {
            value <- index_nodes[[value]]
        }
        normalize_node(value)
    }, character(1L), USE.NAMES = FALSE)
    utils::head(unique(nodes), max_nodes)
}

# Build one global Dataset-only request. Multiple variable_id values are an
# ESGF OR filter; the local discovery evaluator supplies the required AND logic.
daily_probe__discovery_request <- function(models, member, experiments,
                                           activity, variables, node,
                                           data_node = NULL) {
    filters <- list(
        activity_id = activity,
        table_id = "day",
        latest = TRUE,
        replica = FALSE,
        # Restrict the response to stable fields needed by discovery. Some
        # nodes add experimental fields that older ESGF dictionaries reject.
        fields = c(
            "id", "source_id", "experiment_id", "variant_label",
            "member_id", "frequency", "table_id", "variable_id",
            "grid_label", "datetime_start", "data_node", "index_node",
            "instance_id", "master_id", "version", "latest", "replica",
            "number_of_files", "size", "access"
        )
    )
    if (!is.null(data_node)) {
        filters$data_node <- data_node
    }
    shift_request(
        provider = "esgf",
        project = "CMIP6",
        source = models,
        experiment = experiments,
        variant = member,
        variables = variables,
        frequency = "day",
        filters = filters,
        options = list(index_node = node)
    )
}

# Collect global Dataset metadata for one period role without expanding any
# Dataset into File records.
daily_probe__discover_side <- function(models, period_role, experiments,
                                       activity, variables, member, node,
                                       data_node, store_path, ui) {
    request <- daily_probe__discovery_request(
        models = models,
        member = member,
        experiments = experiments,
        activity = activity,
        variables = variables,
        node = node,
        data_node = data_node
    )
    datasets <- shift_datasets(
        request,
        all = TRUE,
        limit = FALSE,
        store = store_path,
        ui = ui
    )
    catalog <- data.table::as.data.table(datasets$to_data_table())
    catalog[, `:=`(
        period_role = rep(period_role, .N),
        discovery_index_node = rep(node, .N)
    )]
    list(
        catalog = catalog,
        dataset_count = as.integer(datasets$count())
    )
}

# Query and locally intersect both period roles at one index node so Dataset
# rows from different nodes can never manufacture a false complete identity.
daily_probe__evaluate_discovery_node <- function(node, config, store_path, ui) {
    variables <- getFromNamespace(
        "daily__input_variables", "epwshiftr")(
            c("core", "enhanced"))
    started <- Sys.time()
    future <- daily_probe__discover_side(
        models = config$models,
        period_role = "future",
        experiments = config$scenarios,
        activity = "ScenarioMIP",
        variables = variables,
        member = config$member,
        node = node,
        data_node = config$data_node,
        store_path = store_path,
        ui = ui
    )
    historical <- daily_probe__discover_side(
        models = config$models,
        period_role = "historical",
        experiments = "historical",
        activity = "CMIP",
        variables = variables,
        member = config$member,
        node = node,
        data_node = config$data_node,
        store_path = store_path,
        ui = ui
    )
    evaluate <- getFromNamespace(
        "daily__evaluate_datasets", "epwshiftr")
    evaluation <- evaluate(
        future_datasets = future$catalog,
        historical_datasets = historical$catalog,
        models = config$models,
        scenarios = config$scenarios,
        future_years = config$future_years,
        historical_years = config$historical_years
    )
    duration <- as.numeric(difftime(Sys.time(), started, units = "secs"))

    list(
        node = node,
        future = future,
        historical = historical,
        evaluation = evaluation,
        duration_seconds = duration
    )
}

# Run Dataset discovery on every configured index node and retain node-local
# evidence plus the models eligible for the more expensive File verification.
daily_probe__run_discovery <- function(nodes, config, store_path, ui) {
    results <- list()
    attempts <- list()
    for (i in seq_along(nodes)) {
        node <- nodes[[i]]
        started_at <- format(
            Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        message(sprintf(
            "Daily CMIP6 Dataset discovery: node %d/%d (%s)",
            i, length(nodes), node
        ))
        result <- tryCatch(
            daily_probe__evaluate_discovery_node(
                node, config, store_path, ui),
            interrupt = function(condition) stop(condition),
            error = identity
        )
        if (inherits(result, "error")) {
            attempts[[length(attempts) + 1L]] <- data.table::data.table(
                attempt = i,
                index_node = node,
                started_at = started_at,
                status = "query_failed",
                duration_seconds = NA_real_,
                future_dataset_count = NA_integer_,
                historical_dataset_count = NA_integer_,
                observed_model_count = NA_integer_,
                core_candidate_count = NA_integer_,
                enhanced_candidate_count = NA_integer_,
                error = conditionMessage(result)
            )
            next
        }

        summary <- data.table::as.data.table(result$evaluation$summary)
        has_summary_contract <- all(
            c("profile", "complete", "source_id") %in% names(summary))
        core_candidate_count <- if (has_summary_contract) {
            data.table::uniqueN(
                summary[profile == "core" & complete]$source_id)
        } else {
            0L
        }
        enhanced_candidate_count <- if (has_summary_contract) {
            data.table::uniqueN(
                summary[profile == "enhanced" & complete]$source_id)
        } else {
            0L
        }
        results[[node]] <- result
        attempts[[length(attempts) + 1L]] <- data.table::data.table(
            attempt = i,
            index_node = node,
            started_at = started_at,
            status = "collected",
            duration_seconds = result$duration_seconds,
            future_dataset_count = result$future$dataset_count,
            historical_dataset_count = result$historical$dataset_count,
            observed_model_count = length(result$evaluation$models),
            core_candidate_count = core_candidate_count,
            enhanced_candidate_count = enhanced_candidate_count,
            error = NA_character_
        )
    }
    attempt_table <- data.table::rbindlist(
        attempts, use.names = TRUE, fill = TRUE)
    if (!length(results)) {
        errors <- attempt_table$error
        errors <- errors[!is.na(errors) & nzchar(errors)]
        stop(
            paste(
                "Dataset discovery failed on every configured ESGF index node.",
                paste(unique(errors), collapse = " | ")
            ),
            call. = FALSE
        )
    }

    evaluations <- lapply(results, function(result) {
        daily_probe__annotate_evaluation(
            result$evaluation[c(
                "candidates", "intersections", "summary",
                "variable_coverage"
            )],
            result$node
        )
    })
    summaries <- data.table::rbindlist(
        lapply(evaluations, `[[`, "summary"),
        use.names = TRUE,
        fill = TRUE
    )
    candidate_nodes <- if (all(
        c("profile", "complete", "source_id", "index_node") %in%
            names(summaries)
    )) {
        unique(summaries[
            profile == "core" & complete,
            .(source_id, index_node)
        ])
    } else {
        data.table::data.table(
            source_id = character(),
            index_node = character()
        )
    }
    node_order <- stats::setNames(seq_along(nodes), nodes)
    candidate_nodes[, node_order := unname(node_order[index_node])]
    data.table::setorderv(
        candidate_nodes, c("source_id", "node_order"))
    candidate_nodes[, node_order := NULL]

    list(
        results = results,
        attempts = attempt_table,
        evaluations = evaluations,
        summaries = summaries,
        candidate_nodes = candidate_nodes,
        models = sort(unique(candidate_nodes$source_id))
    )
}

# Build one role-specific File request with DRS time repair.
daily_probe__request <- function(model, member, experiments, years, activity,
                                 variables, node, data_node = NULL) {
    time_window <- getFromNamespace(
        "shift_time_window", "epwshiftr")(range(years))
    shift_cmip6_scenario(
        source = model,
        scenario = experiments,
        member = member,
        # Dataset-level time facets are unreliable across ESGF indexes. Collect
        # the matching Dataset identities first and filter their File records.
        years = NULL,
        variables = variables,
        frequency = "day",
        activity = activity,
        table_id = "day",
        data_node = data_node,
        index_node = node,
        options = list(
            file_time = time_window,
            time_filter_method = "auto"
        )
    )
}

# Collect Dataset and File metadata for one period role without downloading any
# NetCDF payload or opening an OPeNDAP data array.
daily_probe__collect_side <- function(model, period_role, experiments, years,
                                      activity, variables, member, node, data_node,
                                      store_path, ui) {
    request <- daily_probe__request(
        model = model,
        member = member,
        experiments = experiments,
        years = years,
        activity = activity,
        variables = variables,
        node = node,
        data_node = data_node
    )
    stage <- shift_collect(
        request,
        store = store_path,
        fields = "*",
        all = TRUE,
        limit = FALSE,
        label = sprintf("daily-probe-%s-%s", model, period_role),
        ui = ui
    )
    store <- shift_store(stage)
    on.exit(try(store$close(), silent = TRUE), add = TRUE)
    query_id <- shift_ids(stage)$query_id[[1L]]
    catalog_reader <- getFromNamespace("shift_file_catalog", "epwshiftr")
    catalog <- catalog_reader(store, query_id)
    store$close()
    try(shift_complete(stage), silent = TRUE)

    list(
        catalog = data.table::as.data.table(catalog),
        dataset_count = as.integer(stage@meta$dataset_count),
        file_count = as.integer(stage@meta$file_count),
        query_id = as.character(query_id)
    )
}

# Convert one successful node query into the shared offline evaluation tables.
daily_probe__evaluate_node <- function(model, node, config, store_path, ui) {
    variables <- getFromNamespace(
        "daily__input_variables", "epwshiftr")(
            c("core", "enhanced"))
    started <- Sys.time()
    future <- daily_probe__collect_side(
        model = model,
        period_role = "future",
        experiments = config$scenarios,
        years = config$future_years,
        activity = "ScenarioMIP",
        variables = variables,
        member = config$member,
        node = node,
        data_node = config$data_node,
        store_path = store_path,
        ui = ui
    )
    historical <- daily_probe__collect_side(
        model = model,
        period_role = "historical",
        experiments = "historical",
        years = config$historical_years,
        activity = "CMIP",
        variables = variables,
        member = config$member,
        node = node,
        data_node = config$data_node,
        store_path = store_path,
        ui = ui
    )
    evaluate <- getFromNamespace(
        "daily__evaluate_catalogs", "epwshiftr")
    evaluation <- evaluate(
        future_catalog = future$catalog,
        historical_catalog = historical$catalog,
        models = model,
        scenarios = config$scenarios,
        future_years = config$future_years,
        historical_years = config$historical_years
    )
    duration <- as.numeric(difftime(Sys.time(), started, units = "secs"))

    list(
        node = node,
        future = future,
        historical = historical,
        evaluation = evaluation,
        duration_seconds = duration
    )
}

# Summarize one successful attempt into a lexicographically comparable score.
daily_probe__attempt_score <- function(attempt) {
    summary <- data.table::as.data.table(attempt$evaluation$summary)
    enhanced <- summary[profile == "enhanced"]
    core <- summary[profile == "core"]
    missing <- suppressWarnings(min(summary$missing_total, na.rm = TRUE))
    if (!is.finite(missing)) {
        missing <- 2000
    }
    c(
        enhanced_complete = as.integer(any(enhanced$complete %in% TRUE)),
        core_complete = as.integer(any(core$complete %in% TRUE)),
        both_present = sum(summary$future_present & summary$historical_present),
        missing_score = -as.numeric(missing),
        file_count = attempt$future$file_count +
            attempt$historical$file_count
    )
}

# Compare attempt scores from most important scientific criterion to least.
daily_probe__score_is_better <- function(candidate, current) {
    if (is.null(current)) {
        return(TRUE)
    }
    difference <- candidate - current
    first <- which(difference != 0)
    length(first) > 0L && difference[[first[[1L]]]] > 0
}

# Query ordered nodes for one model, retaining the closest result and stopping
# as soon as the enhanced historical/future identity contract is complete.
daily_probe__run_model <- function(model, nodes, config, store_path, ui) {
    attempts <- list()
    selected <- NULL
    selected_score <- NULL

    for (i in seq_along(nodes)) {
        node <- nodes[[i]]
        started_at <- format(
            Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
        result <- tryCatch(
            daily_probe__evaluate_node(
                model, node, config, store_path, ui),
            interrupt = function(condition) stop(condition),
            error = identity
        )
        if (inherits(result, "error")) {
            attempts[[length(attempts) + 1L]] <- data.table::data.table(
                source_id = model,
                attempt = i,
                index_node = node,
                started_at = started_at,
                status = "query_failed",
                selected = FALSE,
                duration_seconds = NA_real_,
                future_dataset_count = NA_integer_,
                future_file_count = NA_integer_,
                historical_dataset_count = NA_integer_,
                historical_file_count = NA_integer_,
                core_complete = FALSE,
                enhanced_complete = FALSE,
                error = conditionMessage(result)
            )
            next
        }

        score <- daily_probe__attempt_score(result)
        attempts[[length(attempts) + 1L]] <- data.table::data.table(
            source_id = model,
            attempt = i,
            index_node = node,
            started_at = started_at,
            status = "collected",
            selected = FALSE,
            duration_seconds = result$duration_seconds,
            future_dataset_count = result$future$dataset_count,
            future_file_count = result$future$file_count,
            historical_dataset_count = result$historical$dataset_count,
            historical_file_count = result$historical$file_count,
            core_complete = score[["core_complete"]] == 1,
            enhanced_complete = score[["enhanced_complete"]] == 1,
            error = NA_character_
        )
        if (daily_probe__score_is_better(score, selected_score)) {
            selected <- result
            selected_score <- score
        }
        if (score[["enhanced_complete"]] == 1) {
            break
        }
    }

    attempt_table <- data.table::rbindlist(
        attempts, use.names = TRUE, fill = TRUE)
    if (!is.null(selected)) {
        # Capture outside data.table evaluation because the attempts table also
        # has a logical column named `selected`.
        selected_node <- selected$node
        attempt_table[
            index_node == selected_node & status == "collected",
            selected := TRUE
        ]
    }
    list(selected = selected, attempts = attempt_table)
}

# Create typed query-failure rows so failed models remain visible in the final
# summary instead of disappearing from the scientific availability matrix.
daily_probe__failed_evaluation <- function(model, config, message) {
    evaluate <- getFromNamespace(
        "daily__evaluate_catalogs", "epwshiftr")
    evaluation <- evaluate(
        future_catalog = data.table::data.table(),
        historical_catalog = data.table::data.table(),
        models = model,
        scenarios = config$scenarios,
        future_years = config$future_years,
        historical_years = config$historical_years
    )
    evaluation$summary[, `:=`(
        status = "query_failed",
        future_missing = message,
        historical_missing = message
    )]
    evaluation$intersections[, `:=`(
        status = "query_failed",
        future_missing = message,
        historical_missing = message
    )]
    evaluation
}

# Tag every selected table with its index-node provenance before aggregation.
daily_probe__annotate_evaluation <- function(evaluation, node) {
    for (name in names(evaluation)) {
        table <- data.table::as.data.table(evaluation[[name]])
        table[, index_node := rep(node, .N)]
        evaluation[[name]] <- table
    }
    evaluation
}

# Reduce Dataset discovery records to the identity, coverage, and provenance
# fields needed to reproduce the global model shortlist.
daily_probe__dataset_receipt <- function(catalog) {
    catalog <- data.table::as.data.table(data.table::copy(catalog))
    n <- nrow(catalog)
    character_column <- function(name) {
        value <- catalog[[name]]
        if (is.null(value)) {
            return(rep(NA_character_, n))
        }
        as.character(value)
    }
    variant <- character_column("variant_label")
    member <- character_column("member_id")
    use_member <- (is.na(variant) | !nzchar(variant)) &
        !is.na(member) & nzchar(member)
    variant[use_member] <- member[use_member]
    catalog[["variant_label"]] <- variant

    fields <- c(
        "period_role", "discovery_index_node", "source_id",
        "experiment_id", "variant_label", "frequency", "table_id",
        "variable_id", "grid_label", "datetime_start", "datetime_stop",
        "data_node", "instance_id", "master_id", "version", "latest",
        "replica", "number_of_files", "size"
    )
    for (field in setdiff(fields, names(catalog))) {
        catalog[[field]] <- NA
    }
    unique(catalog[, ..fields])
}

# Reduce a stored File catalog to the auditable fields needed by this survey.
daily_probe__catalog_receipt <- function(catalog, period_role, node) {
    catalog <- data.table::as.data.table(data.table::copy(catalog))
    fields <- c(
        "source_id", "experiment_id", "variant_label", "frequency",
        "table_id", "variable_id", "grid_label", "datetime_start",
        "datetime_end", "data_node", "filename", "size", "latest",
        "replica", "retracted", "deprecated", "url_opendap", "url_download"
    )
    for (field in setdiff(fields, names(catalog))) {
        catalog[[field]] <- NA
    }
    has_access <- getFromNamespace("daily__has_access", "epwshiftr")
    catalog[, `:=`(
        period_role = rep(period_role, .N),
        index_node = rep(node, .N),
        has_opendap = has_access(catalog, "url_opendap"),
        has_http = has_access(catalog, "url_download")
    )]
    output_fields <- c(
        "period_role", "index_node",
        setdiff(fields, c("url_opendap", "url_download")),
        "has_opendap", "has_http"
    )
    catalog[, ..output_fields][]
}

# Hash one receipt file with the package's existing SHA-256 implementation.
daily_probe__hash_file <- function(path) {
    hash_file <- getFromNamespace("store_hash_file", "epwshiftr")
    unname(hash_file(path, "sha256"))
}

# Write deterministic CSV/JSON receipts and a checksum manifest outside the
# mutable DuckDB metadata store.
daily_probe__write_receipt <- function(result, config, package_root,
                                       output, store_path, nodes) {
    if (!dir.exists(output)) {
        ok <- dir.create(output, recursive = TRUE, showWarnings = FALSE)
        if (!isTRUE(ok) && !dir.exists(output)) {
            stop(sprintf("Cannot create output directory: %s", output),
                call. = FALSE)
        }
    }

    tables <- list(
        "model-summary.csv" = result$summary,
        "candidate-intersections.csv" = result$intersections,
        "period-candidates.csv" = result$candidates,
        "variable-coverage.csv" = result$variable_coverage,
        "file-query-attempts.csv" = result$attempts,
        "file-catalog.csv" = result$catalog,
        "dataset-discovery-summary.csv" = result$discovery_summary,
        "dataset-discovery-intersections.csv" =
            result$discovery_intersections,
        "dataset-discovery-candidates.csv" =
            result$discovery_candidates,
        "dataset-discovery-attempts.csv" =
            result$discovery_attempts,
        "dataset-catalog.csv" = result$discovery_catalog
    )
    paths <- file.path(output, names(tables))
    for (i in seq_along(tables)) {
        data.table::fwrite(
            data.table::as.data.table(tables[[i]]),
            paths[[i]],
            na = "NA",
            dateTimeAs = "ISO",
            bom = FALSE
        )
    }

    git_commit <- suppressWarnings(system2(
        "git",
        c("-C", shQuote(package_root), "rev-parse", "HEAD"),
        stdout = TRUE,
        stderr = FALSE
    ))
    git_commit <- if (length(git_commit)) git_commit[[1L]] else NA_character_
    files <- lapply(seq_along(paths), function(i) {
        list(
            path = basename(paths[[i]]),
            rows = nrow(tables[[i]]),
            sha256 = daily_probe__hash_file(paths[[i]])
        )
    })
    names(files) <- NULL
    summary <- data.table::as.data.table(result$summary)
    discovery_attempts <- data.table::as.data.table(
        result$discovery_attempts)
    file_attempts <- data.table::as.data.table(result$attempts)
    has_query_failures <-
        (nrow(summary) &&
            "status" %in% names(summary) &&
            any(summary$status == "query_failed")) ||
        (nrow(file_attempts) &&
            "status" %in% names(file_attempts) &&
            any(file_attempts$status == "query_failed")) ||
        (nrow(discovery_attempts) &&
            any(discovery_attempts$status == "query_failed"))
    manifest <- list(
        schema_version = 2L,
        status = if (has_query_failures) {
            "completed_with_query_failures"
        } else {
            "completed"
        },
        created_at = format(
            Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        metadata_only = TRUE,
        source = list(
            package = "epwshiftr",
            package_version = as.character(
                utils::packageVersion("epwshiftr")),
            git_commit = git_commit,
            package_root = package_root
        ),
        request = list(
            model_scope = if (is.null(config$models)) {
                "all_discovered"
            } else {
                "explicit"
            },
            models = if (is.null(config$models)) {
                NULL
            } else {
                as.list(config$models)
            },
            scenarios = as.list(config$scenarios),
            member = config$member,
            future_years = range(config$future_years),
            historical_years = range(config$historical_years),
            frequency = "day",
            table_id = "day",
            data_node = config$data_node,
            index_nodes = as.list(nodes),
            query_timeout_seconds = config$query_timeout,
            connect_timeout_seconds = config$connect_timeout,
            discovery = list(
                level = "Dataset",
                time_constraint = "none_presence_only",
                variable_filter_semantics = "OR",
                local_requirement_semantics = "AND_with_humidity_alternative",
                file_expansion = "core_candidates_only"
            ),
            profiles = lapply(c("core", "enhanced"), function(profile) {
                list(
                    name = profile,
                    requirements = getFromNamespace(
                        "daily__requirements", "epwshiftr")(profile)
                )
            })
        ),
        result = list(
            discovered_model_count =
                if ("source_id" %in% names(result$discovery_summary)) {
                    data.table::uniqueN(
                        result$discovery_summary$source_id)
                } else {
                    0L
                },
            candidate_model_count = length(result$candidate_models),
            verified_model_count = if (nrow(summary)) {
                data.table::uniqueN(summary$source_id)
            } else {
                0L
            },
            core_complete_models = if (nrow(summary)) {
                sum(summary[profile == "core"]$complete %in% TRUE)
            } else {
                0L
            },
            enhanced_complete_models = if (nrow(summary)) {
                sum(summary[profile == "enhanced"]$complete %in% TRUE)
            } else {
                0L
            },
            query_failed_models = if (nrow(summary)) {
                data.table::uniqueN(
                    summary[status == "query_failed"]$source_id)
            } else {
                0L
            }
        ),
        store = normalizePath(
            store_path, winslash = "/", mustWork = FALSE),
        files = files
    )
    manifest_path <- file.path(output, "manifest.json")
    jsonlite::write_json(
        manifest,
        manifest_path,
        auto_unbox = TRUE,
        pretty = TRUE,
        null = "null",
        na = "null",
        digits = 16
    )

    checksum_paths <- c(paths, manifest_path)
    checksum_names <- basename(checksum_paths)
    order <- order(checksum_names)
    writeLines(
        sprintf(
            "%s  %s",
            vapply(checksum_paths[order], daily_probe__hash_file,
                character(1L)),
            checksum_names[order]
        ),
        file.path(output, "checksums.sha256"),
        useBytes = TRUE
    )
    manifest
}

# Bind one named evaluation component while preserving a valid empty table when
# Dataset discovery produces no File-verification candidates.
daily_probe__bind_evaluations <- function(evaluations, component) {
    if (!length(evaluations)) {
        return(data.table::data.table())
    }
    data.table::rbindlist(
        lapply(evaluations, `[[`, component),
        use.names = TRUE,
        fill = TRUE
    )
}

# Execute global Dataset discovery first, then expand File metadata only for
# models that satisfy the core variable and identity contract.
daily_probe__execute <- function(config, package_root) {
    nodes <- daily_probe__resolve_nodes(
        config$index_nodes, config$max_nodes)
    output <- normalizePath(
        config$output, winslash = "/", mustWork = FALSE)
    store_path <- file.path(output, "store")
    if (isTRUE(config$plan)) {
        jsonlite::write_json(
            list(
                model_scope = if (is.null(config$models)) {
                    "all_discovered"
                } else {
                    "explicit"
                },
                models = config$models,
                scenarios = config$scenarios,
                member = config$member,
                future_years = range(config$future_years),
                historical_years = range(config$historical_years),
                index_nodes = nodes,
                query_timeout_seconds = config$query_timeout,
                connect_timeout_seconds = config$connect_timeout,
                data_node = config$data_node,
                output = output,
                metadata_only = TRUE,
                query_strategy = c(
                    "global_Dataset_discovery",
                    "local_variable_intersection",
                    "candidate_File_verification"
                )
            ),
            stdout(),
            auto_unbox = TRUE,
            pretty = TRUE,
            null = "null"
        )
        cat("\n")
        return(invisible(NULL))
    }
    dir.create(store_path, recursive = TRUE, showWarnings = FALSE)
    old_options <- options(
        epwshiftr.query.timeout = config$query_timeout,
        epwshiftr.query.connect_timeout = config$connect_timeout
    )
    on.exit(options(old_options), add = TRUE)
    ui <- shift_ui(
        progress = config$ui,
        detail = "normal",
        motion = "none",
        refresh = 0.2,
        heartbeat = 10
    )

    discovery <- daily_probe__run_discovery(
        nodes, config, store_path, ui)
    message(sprintf(
        paste(
            "Daily CMIP6 Dataset discovery found %d core candidate",
            "model%s for File verification."
        ),
        length(discovery$models),
        if (length(discovery$models) == 1L) "" else "s"
    ))

    model_results <- vector("list", length(discovery$models))
    names(model_results) <- discovery$models
    for (model in discovery$models) {
        model_nodes <- discovery$candidate_nodes[
            source_id == model, index_node]
        message(sprintf(
            "Daily CMIP6 File verification: %s (%d candidate node%s)",
            model, length(model_nodes),
            if (length(model_nodes) == 1L) "" else "s"
        ))
        model_results[[model]] <- daily_probe__run_model(
            model, model_nodes, config, store_path, ui)
    }

    evaluations <- list()
    catalogs <- list()
    attempts <- list()
    for (model in names(model_results)) {
        model_result <- model_results[[model]]
        attempts[[model]] <- model_result$attempts
        selected <- model_result$selected
        if (is.null(selected)) {
            errors <- model_result$attempts$error
            errors <- errors[!is.na(errors) & nzchar(errors)]
            message <- if (length(errors)) {
                paste(unique(errors), collapse = " | ")
            } else {
                "All configured ESGF index-node queries failed."
            }
            evaluations[[model]] <- daily_probe__annotate_evaluation(
                daily_probe__failed_evaluation(model, config, message),
                NA_character_
            )
            next
        }
        evaluations[[model]] <- daily_probe__annotate_evaluation(
            selected$evaluation, selected$node)
        catalogs[[paste0(model, "-future")]] <-
            daily_probe__catalog_receipt(
                selected$future$catalog, "future", selected$node)
        catalogs[[paste0(model, "-historical")]] <-
            daily_probe__catalog_receipt(
                selected$historical$catalog, "historical", selected$node)
    }

    discovery_evaluations <- discovery$evaluations
    discovery_catalogs <- lapply(
        discovery$results,
        function(node_result) {
            data.table::rbindlist(list(
                daily_probe__dataset_receipt(
                    node_result$future$catalog),
                daily_probe__dataset_receipt(
                    node_result$historical$catalog)
            ), use.names = TRUE, fill = TRUE)
        }
    )
    result <- list(
        summary = daily_probe__bind_evaluations(
            evaluations, "summary"),
        intersections = daily_probe__bind_evaluations(
            evaluations, "intersections"),
        candidates = daily_probe__bind_evaluations(
            evaluations, "candidates"),
        variable_coverage = daily_probe__bind_evaluations(
            evaluations, "variable_coverage"),
        attempts = data.table::rbindlist(
            attempts, use.names = TRUE, fill = TRUE),
        catalog = data.table::rbindlist(
            catalogs, use.names = TRUE, fill = TRUE),
        discovery_summary = daily_probe__bind_evaluations(
            discovery_evaluations, "summary"),
        discovery_intersections = daily_probe__bind_evaluations(
            discovery_evaluations, "intersections"),
        discovery_candidates = daily_probe__bind_evaluations(
            discovery_evaluations, "candidates"),
        discovery_attempts = discovery$attempts,
        discovery_catalog = data.table::rbindlist(
            discovery_catalogs, use.names = TRUE, fill = TRUE)
    )
    for (name in names(result)) {
        table <- data.table::as.data.table(result[[name]])
        common_order <- intersect(
            c(
                "profile", "source_id", "candidate_rank", "period_role",
                "experiment_id", "variable_id", "attempt"
            ),
            names(table)
        )
        if (length(common_order) && nrow(table)) {
            data.table::setorderv(table, common_order, na.last = TRUE)
        }
        result[[name]] <- table
    }
    result$candidate_models <- discovery$models
    manifest <- daily_probe__write_receipt(
        result, config, package_root, output, store_path, nodes)
    cat(sprintf(
        "Daily CMIP6 receipt: %s (%s)\n",
        manifest$status,
        normalizePath(output, winslash = "/", mustWork = TRUE)
    ))
    invisible(manifest)
}

# Keep script execution separate from sourcing so offline tests can exercise
# parsing and selection helpers without contacting ESGF.
daily_probe__main <- function(args = commandArgs(trailingOnly = TRUE)) {
    config <- daily_probe__parse_args(args)
    if (isTRUE(config$help)) {
        cat(daily_probe__usage(), "\n")
        return(invisible(NULL))
    }
    package_root <- daily_probe__package_root()
    daily_probe__load_package(package_root)
    options(timeout = max(getOption("timeout"), 1200))
    daily_probe__execute(config, package_root)
}

if (sys.nframe() == 0L) {
    tryCatch(
        daily_probe__main(),
        error = function(error) {
            message("Daily CMIP6 probe failed: ", conditionMessage(error))
            quit(save = "no", status = 1L)
        }
    )
}
