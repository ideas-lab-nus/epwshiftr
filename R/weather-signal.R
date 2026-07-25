#' @include weather-component.R
NULL

# Signal profiles distinguish defaults supported by a published method from
# experimental defaults introduced by an implementation.
SIGNAL_PROFILE_EVIDENCE <- c("published", "experimental")

# Signal execution either aborts at the first failed group or returns the
# failure explicitly alongside successful group results.
SIGNAL_ERROR_POLICIES <- c("abort", "collect")

# SignalVariableProfile records variable-specific defaults without embedding
# them in an algorithm function or losing their evidence status.
SignalVariableProfile <- S7::new_class(
    "SignalVariableProfile",
    properties = list(
        variable_id = S7::new_property(S7::class_character),
        evidence = S7::new_property(S7::class_character),
        settings = S7::new_property(S7::class_list),
        references = S7::new_property(
            S7::class_character,
            default = character()
        ),
        metadata = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        if (length(self@variable_id) != 1L ||
            is.na(self@variable_id) ||
            !grepl("^[A-Za-z][A-Za-z0-9_]*$", self@variable_id)) {
            return("`variable_id` must be one CMIP-style variable identifier.")
        }
        if (length(self@evidence) != 1L ||
            is.na(self@evidence) ||
            !self@evidence %in% SIGNAL_PROFILE_EVIDENCE) {
            return(sprintf(
                "`evidence` must be one of %s.",
                paste(
                    sprintf("`%s`", SIGNAL_PROFILE_EVIDENCE),
                    collapse = ", "
                )
            ))
        }
        if (length(self@settings) &&
            (is.null(names(self@settings)) ||
                any(!nzchar(names(self@settings))) ||
                anyDuplicated(names(self@settings)))) {
            return("`settings` must be a uniquely named list.")
        }
        if (length(self@metadata) &&
            (is.null(names(self@metadata)) ||
                any(!nzchar(names(self@metadata))) ||
                anyDuplicated(names(self@metadata)))) {
            return("`metadata` must be a uniquely named list.")
        }
        if (anyNA(self@references) ||
            any(!nzchar(self@references)) ||
            anyDuplicated(self@references)) {
            return(
                "`references` must contain unique, non-missing, non-empty values."
            )
        }
        if (identical(self@evidence, "published") &&
            !length(self@references)) {
            return(
                "Published variable profiles must provide at least one reference."
            )
        }
        NULL
    }
)

# SignalGroup carries one already aligned unit of work. Calendar mapping and
# source alignment happen before this boundary so kernels never infer dates.
SignalGroup <- S7::new_class(
    "SignalGroup",
    properties = list(
        key = S7::new_property(S7::class_list, default = list()),
        inputs = S7::new_property(S7::class_list),
        variables = S7::new_property(S7::class_character)
    ),
    validator = function(self) {
        if (length(self@key)) {
            if (is.null(names(self@key)) ||
                any(!nzchar(names(self@key))) ||
                anyDuplicated(names(self@key)) ||
                any(vapply(self@key, length, integer(1L)) != 1L) ||
                any(!vapply(self@key, is.atomic, logical(1L)))) {
                return(
                    "`key` must be a uniquely named list of atomic scalar values."
                )
            }
        }
        if (!length(self@inputs) ||
            is.null(names(self@inputs)) ||
            any(!nzchar(names(self@inputs))) ||
            anyDuplicated(names(self@inputs))) {
            return("`inputs` must be a non-empty, uniquely named role list.")
        }
        if (!all(names(self@inputs) %in% WEATHER_INPUT_ROLES)) {
            return("`inputs` contains an unknown future-weather input role.")
        }
        if (any(vapply(self@inputs, is.null, logical(1L)))) {
            return("`inputs` cannot contain NULL role payloads.")
        }
        if (!length(self@variables) ||
            anyNA(self@variables) ||
            any(!nzchar(self@variables)) ||
            anyDuplicated(self@variables) ||
            any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", self@variables))) {
            return(
                "`variables` must contain unique, non-missing variable IDs."
            )
        }
        NULL
    }
)

# SignalExecutionResult keeps group outputs positionally aligned with their
# inputs and records a status row for every attempted group.
SignalExecutionResult <- S7::new_class(
    "SignalExecutionResult",
    properties = list(
        groups = S7::new_property(S7::class_list),
        values = S7::new_property(S7::class_list),
        profiles = S7::new_property(S7::class_list),
        diagnostics = S7::new_property(S7::class_any)
    ),
    validator = function(self) {
        if (!all(vapply(
            self@groups,
            S7::S7_inherits,
            logical(1L),
            class = SignalGroup
        ))) {
            return("`groups` must contain only SignalGroup objects.")
        }
        if (length(self@values) != length(self@groups)) {
            return("`values` must remain positionally aligned with `groups`.")
        }
        variables <- unique(unlist(
            lapply(self@groups, function(group) group@variables),
            use.names = FALSE
        ))
        if (is.null(names(self@profiles)) ||
            !setequal(names(self@profiles), variables)) {
            return(
                "`profiles` must record the resolved settings for every group variable."
            )
        }
        expected <- c(
            "method", "group", "status", "variables", "evidence", "message"
        )
        if (!is.data.frame(self@diagnostics) ||
            !identical(names(self@diagnostics), expected) ||
            nrow(self@diagnostics) != length(self@groups)) {
            return(
                "`diagnostics` must contain one canonical row per signal group."
            )
        }
        if (any(!self@diagnostics[["status"]] %in% c("ok", "error"))) {
            return("Signal diagnostic status must be `ok` or `error`.")
        }
        NULL
    }
)

# Construct one variable profile and enforce that published defaults cite their
# source while experimental defaults remain visibly labelled.
signal__variable_profile <- function(
    variable_id,
    settings = list(),
    evidence = c("published", "experimental"),
    references = character(),
    metadata = list()
) {
    checkmate::assert_string(
        variable_id,
        pattern = "^[A-Za-z][A-Za-z0-9_]*$"
    )
    evidence <- match.arg(evidence)
    checkmate::assert_list(settings, names = "unique")
    if (length(settings) &&
        (is.null(names(settings)) || any(!nzchar(names(settings))))) {
        cli::cli_abort("{.arg settings} must be named.")
    }
    references <- weather__descriptor_values(references, "references")
    checkmate::assert_list(metadata, names = "unique")

    SignalVariableProfile(
        variable_id = variable_id,
        evidence = evidence,
        settings = settings,
        references = references,
        metadata = metadata
    )
}

# Normalize profile collections by variable ID so construction order never
# changes profile lookup or serialized metadata.
signal__profiles <- function(profiles) {
    checkmate::assert_list(profiles)
    if (!length(profiles)) {
        cli::cli_abort("{.arg profiles} must contain at least one variable profile.")
    }
    if (!all(vapply(
        profiles,
        S7::S7_inherits,
        logical(1L),
        class = SignalVariableProfile
    ))) {
        cli::cli_abort(
            "{.arg profiles} must contain only SignalVariableProfile objects."
        )
    }
    ids <- vapply(
        profiles,
        function(profile) profile@variable_id,
        character(1L)
    )
    if (anyDuplicated(ids)) {
        cli::cli_abort(
            "{.arg profiles} contains duplicate variable IDs: {.val {unique(ids[duplicated(ids)])}}."
        )
    }
    stats::setNames(profiles, ids)
}

# Normalize variable-specific user overrides before applying them to profile
# defaults. Overrides may change settings but not their evidence provenance.
signal__overrides <- function(overrides, variables) {
    checkmate::assert_list(overrides, names = "unique")
    if (!length(overrides)) {
        return(stats::setNames(
            rep(list(list()), length(variables)),
            variables
        ))
    }
    if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) {
        cli::cli_abort(
            "{.arg overrides} must be named by variable ID."
        )
    }
    unknown <- setdiff(names(overrides), variables)
    if (length(unknown)) {
        cli::cli_abort(
            "{.arg overrides} contains variable(s) not present in the signal groups: {.val {unknown}}."
        )
    }
    if (!all(vapply(overrides, is.list, logical(1L)))) {
        cli::cli_abort(
            "Every {.arg overrides} entry must be a named settings list."
        )
    }
    for (variable in names(overrides)) {
        checkmate::assert_list(
            overrides[[variable]],
            names = "unique"
        )
        if (length(overrides[[variable]]) &&
            (is.null(names(overrides[[variable]])) ||
                any(!nzchar(names(overrides[[variable]]))))) {
            cli::cli_abort(
                "Override settings for {.val {variable}} must be named."
            )
        }
    }
    out <- stats::setNames(
        rep(list(list()), length(variables)),
        variables
    )
    out[names(overrides)] <- overrides
    out
}

# Resolve every variable once per execution so experimental warnings are not
# repeated for each location or temporal window.
signal__resolve_profiles <- function(
    profiles,
    variables,
    overrides = list(),
    warn_experimental = TRUE
) {
    profiles <- signal__profiles(profiles)
    variables <- weather__descriptor_values(variables, "variables")
    checkmate::assert_flag(warn_experimental)
    missing <- setdiff(variables, names(profiles))
    if (length(missing)) {
        cli::cli_abort(
            "No signal variable profile is registered for {.val {missing}}."
        )
    }
    overrides <- signal__overrides(overrides, variables)

    resolved <- lapply(variables, function(variable) {
        profile <- profiles[[variable]]
        if (isTRUE(warn_experimental) &&
            identical(profile@evidence, "experimental")) {
            cli::cli_warn(c(
                "Signal defaults for {.val {variable}} are experimental.",
                "i" = "Review the method settings and resulting diagnostics."
            ))
        }
        list(
            profile = profile,
            settings = utils::modifyList(
                profile@settings,
                overrides[[variable]],
                keep.null = TRUE
            )
        )
    })
    stats::setNames(resolved, variables)
}

# Construct one pre-aligned group without imposing a package-wide array or
# table representation on individual signal methods.
signal__group <- function(key = list(), inputs, variables) {
    checkmate::assert_list(key, names = "unique")
    checkmate::assert_list(inputs, names = "unique", min.len = 1L)
    variables <- weather__descriptor_values(variables, "variables")

    SignalGroup(
        key = key,
        inputs = inputs,
        variables = variables
    )
}

# Render a deterministic group label for diagnostics without requiring every
# upstream adapter to invent a string identifier.
signal__group_label <- function(group, index) {
    if (!length(group@key)) {
        return(sprintf("group-%d", index))
    }
    values <- vapply(group@key, as.character, character(1L))
    paste(sprintf("%s=%s", names(values), values), collapse = ",")
}

# Validate the role boundary again at group granularity because a globally
# available source can still be absent after alignment for one location.
signal__validate_group_roles <- function(component, group) {
    required <- names(component@required_inputs)
    optional <- names(component@optional_inputs)
    missing <- setdiff(required, names(group@inputs))
    if (length(missing)) {
        cli::cli_abort(
            "Signal group is missing required input role(s): {.val {missing}}."
        )
    }
    unexpected <- setdiff(names(group@inputs), c(required, optional))
    if (length(unexpected)) {
        cli::cli_abort(
            "Signal group contains undeclared input role(s): {.val {unexpected}}."
        )
    }
    invisible(TRUE)
}

# Apply an optional method-specific output validator while keeping the common
# executor independent from numeric-vector, table, or field representations.
signal__validate_result <- function(component, value, group) {
    if (is.null(value)) {
        cli::cli_abort("A signal group kernel cannot return NULL.")
    }
    if (!"validate_result" %in% names(component@operations)) {
        return(invisible(TRUE))
    }
    result <- component@operations$validate_result(
        value = value,
        inputs = group@inputs,
        key = group@key
    )
    if (isTRUE(result)) {
        return(invisible(TRUE))
    }
    if (is.character(result) && length(result) == 1L && !is.na(result)) {
        cli::cli_abort(result)
    }
    cli::cli_abort(
        "A signal result validator must return TRUE or one diagnostic string."
    )
}

# Build one canonical diagnostic row for either a successful or failed group.
signal__diagnostic <- function(
    method,
    group,
    index,
    status,
    profiles,
    message = NA_character_
) {
    data.table::data.table(
        method = method,
        group = signal__group_label(group, index),
        status = status,
        variables = paste(group@variables, collapse = ","),
        evidence = paste(
            unique(vapply(
                profiles[group@variables],
                function(resolved) resolved$profile@evidence,
                character(1L)
            )),
            collapse = ","
        ),
        message = message
    )
}

# Convert resolved profiles into data-only records that retain the actual
# settings used after overrides as well as their original provenance.
signal__profile_records <- function(profiles) {
    lapply(profiles, function(resolved) {
        list(
            variable_id = resolved$profile@variable_id,
            evidence = resolved$profile@evidence,
            settings = resolved$settings,
            references = resolved$profile@references,
            metadata = resolved$profile@metadata
        )
    })
}

# Execute a component's single-group kernel over pre-aligned groups. Failed
# groups remain NULL with explicit diagnostics instead of silently becoming NaN.
signal__execute_groups <- function(
    component,
    inputs,
    groups,
    profiles,
    overrides = list(),
    error_policy = c("abort", "collect"),
    warn_experimental = TRUE
) {
    if (!S7::S7_inherits(component, WeatherComponentSpec) ||
        !identical(component@stage, "signal")) {
        cli::cli_abort(
            "{.arg component} must be a signal WeatherComponentSpec object."
        )
    }
    component__validate_inputs(component, inputs)
    checkmate::assert_list(groups, min.len = 1L)
    if (!all(vapply(
        groups,
        S7::S7_inherits,
        logical(1L),
        class = SignalGroup
    ))) {
        cli::cli_abort("{.arg groups} must contain only SignalGroup objects.")
    }
    error_policy <- match.arg(error_policy)
    variables <- unique(unlist(
        lapply(groups, function(group) group@variables),
        use.names = FALSE
    ))
    resolved <- signal__resolve_profiles(
        profiles,
        variables,
        overrides = overrides,
        warn_experimental = warn_experimental
    )

    values <- vector("list", length(groups))
    diagnostics <- vector("list", length(groups))
    for (i in seq_along(groups)) {
        group <- groups[[i]]
        # Role coverage is checked immediately before the method kernel so
        # alignment failures are attributed to the exact group.
        attempt <- tryCatch(
            {
                signal__validate_group_roles(component, group)
                group_profiles <- resolved[group@variables]
                value <- component@operations$apply_group(
                    inputs = group@inputs,
                    settings = lapply(
                        group_profiles,
                        function(item) item$settings
                    ),
                    key = group@key
                )
                signal__validate_result(component, value, group)
                list(
                    ok = TRUE,
                    value = value,
                    profiles = group_profiles
                )
            },
            error = function(error) {
                list(
                    ok = FALSE,
                    error = error,
                    profiles = resolved[group@variables]
                )
            }
        )
        if (!isTRUE(attempt$ok) &&
            identical(error_policy, "abort")) {
            cli::cli_abort(
                "Signal component {.val {component@name}} failed for {.val {signal__group_label(group, i)}}.",
                parent = attempt$error
            )
        }
        if (isTRUE(attempt$ok)) {
            values[[i]] <- attempt$value
            diagnostics[[i]] <- signal__diagnostic(
                component@name,
                group,
                i,
                "ok",
                attempt$profiles
            )
        } else {
            # Assign with single-bracket indexing so NULL remains an explicit
            # position instead of shortening the values list.
            values[i] <- list(NULL)
            diagnostics[[i]] <- signal__diagnostic(
                component@name,
                group,
                i,
                "error",
                attempt$profiles,
                conditionMessage(attempt$error)
            )
        }
    }

    SignalExecutionResult(
        groups = groups,
        values = values,
        profiles = signal__profile_records(resolved),
        diagnostics = data.table::rbindlist(diagnostics)
    )
}

# Construct a signal component whose public `apply` operation uses the common
# group executor while `apply_group` remains the method-specific kernel.
signal__component <- function(
    name,
    label = name,
    required_inputs,
    optional_inputs = list(),
    input_kinds = "calendar_indexed",
    output_kinds = "signal_adjusted",
    scopes = "univariate",
    stochastic = FALSE,
    profiles,
    apply_group,
    operations = list(),
    metadata = list()
) {
    profiles <- signal__profiles(profiles)
    checkmate::assert_function(apply_group)
    checkmate::assert_list(operations, names = "unique")
    reserved <- intersect(names(operations), c("apply", "apply_group"))
    if (length(reserved)) {
        cli::cli_abort(
            "{.arg operations} cannot replace reserved operation(s): {.val {reserved}}."
        )
    }
    if (!length(required_inputs)) {
        cli::cli_abort(
            "A signal component must declare at least one required input role."
        )
    }
    if ("signal_profiles" %in% names(metadata)) {
        cli::cli_abort(
            "{.arg metadata} cannot replace the reserved `signal_profiles` entry."
        )
    }

    # Executable functions stay process-local, while profile summaries in
    # metadata remain inspectable and serializable.
    metadata$signal_profiles <- lapply(profiles, function(profile) {
        list(
            variable_id = profile@variable_id,
            evidence = profile@evidence,
            settings = profile@settings,
            references = profile@references,
            metadata = profile@metadata
        )
    })
    state <- new.env(parent = emptyenv())
    apply <- function(
        inputs,
        groups,
        overrides = list(),
        error_policy = c("abort", "collect"),
        warn_experimental = TRUE
    ) {
        signal__execute_groups(
            state$component,
            inputs,
            groups,
            profiles = profiles,
            overrides = overrides,
            error_policy = error_policy,
            warn_experimental = warn_experimental
        )
    }
    component <- component__spec(
        name = name,
        stage = "signal",
        label = label,
        required_inputs = required_inputs,
        optional_inputs = optional_inputs,
        input_kinds = input_kinds,
        output_kinds = output_kinds,
        scopes = scopes,
        stochastic = stochastic,
        operations = c(
            list(apply = apply, apply_group = apply_group),
            operations
        ),
        metadata = metadata
    )
    state$component <- component
    component
}
