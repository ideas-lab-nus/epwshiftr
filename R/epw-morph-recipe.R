#' @include backend-registry.R weather-recipe.R
NULL

# EPW morphing recipe contracts {{{

EPW_MORPH_VARIABLE_LEVELS <- list(
    minimal = c("tas", "hurs"),
    recommended = c("tas", "hurs", "psl", "rlds", "rsds", "sfcWind", "clt", "pr"),
    extended = c("tas", "tasmax", "tasmin", "hurs", "hursmax", "hursmin", "psl", "rlds", "rsds", "sfcWind", "clt", "pr", "snd")
)

#' EPW morphing variable sets
#'
#' @param level Variable set level, an [EpwMorphBackend] object, or an
#'        [epw_morph_recipe()] object.
#' @param include_optional Whether to include optional source variables used by
#'   enhanced methods.
#'
#' @return A character vector of CMIP variable IDs.
#' @export
epw_morph_variables <- function(level = c("recommended", "minimal", "extended"),
                                include_optional = FALSE) {
    checkmate::assert_flag(include_optional)
    if (inherits(level, "epw_morph_recipe")) {
        rules <- morpher__recipe_rules(level)
        required <- morpher__rules_required_variables(rules[required == TRUE & !derived])
        if (!isTRUE(include_optional)) {
            return(required)
        }
        optional <- unique(unlist(c(
            rules[required == TRUE & !derived, optional_variables],
            rules[required == FALSE & !derived, required_variables]
        ), use.names = FALSE))
        if (identical(level$backend, "belcher") || identical(level$backend, "belcher_absolute")) {
            if (identical(level$options$snow_depth, "off")) {
                optional <- setdiff(optional, "snd")
            }
        }
        return(unique(c(required, optional)))
    }
    if (inherits(level, "EpwMorphBackend")) {
        required <- level$required_variables()
        if (!isTRUE(include_optional)) {
            return(required)
        }
        rules <- level$rules()
        optional <- unique(unlist(c(
            rules[required == TRUE & !derived, optional_variables],
            rules[required == FALSE & !derived, required_variables]
        ), use.names = FALSE))
        return(unique(c(required, optional)))
    }
    if (is.character(level) && length(level) == 1L && !level %in% names(EPW_MORPH_VARIABLE_LEVELS)) {
        if (tolower(level) %in% epw_morph_recipes()[["name"]]) {
            return(epw_morph_variables(
                epw_morph_recipe(level),
                include_optional = include_optional
            ))
        }
        return(epw_morph_variables(epw_morph_backend(level), include_optional = include_optional))
    }
    level <- match.arg(level)
    variables <- EPW_MORPH_VARIABLE_LEVELS[[level]]
    if (isTRUE(include_optional) && !identical(level, "extended")) {
        variables <- unique(c(variables, setdiff(EPW_MORPH_VARIABLE_LEVELS$extended, variables)))
    }
    variables
}

# Describe canonical morph variables separately from the source-variable
# alternatives that can produce them. Keeping this at the recipe boundary lets
# the resolver and extraction workflow share one capability contract without
# teaching the morphing engine about ESGF catalog details.
morpher__variable_requirements <- function(recipe) {
    canonical <- epw_morph_variables(recipe)
    requirements <- stats::setNames(
        lapply(canonical, function(variable) list(variable)),
        canonical
    )
    if (inherits(recipe, "epw_morph_recipe") &&
        recipe$backend %in% c("belcher", "belcher_absolute") &&
        "hurs" %in% canonical) {
        source <- recipe$options$humidity_source
        rh_method <- recipe$methods[["rh"]]
        # A non-shift RH override requires the original HURS path. Otherwise,
        # enhanced auto mode prefers a complete HUSS state without mixing
        # humidity sources between future and reference periods.
        if (identical(source, "auto") && !identical(rh_method, "shift")) {
            source <- "hurs"
        }
        requirements[["hurs"]] <- if (identical(recipe$profile, "legacy")) {
            list("hurs", c("huss", "tas", "ps"))
        } else if (identical(source, "huss")) {
            list(c("huss", "tas", "ps"))
        } else if (identical(source, "hurs")) {
            list("hurs")
        } else {
            list(c("huss", "tas", "ps"), "hurs")
        }
    }
    if (inherits(recipe, "epw_morph_recipe") &&
        recipe$backend %in% c("belcher", "belcher_absolute") &&
        identical(recipe$options$snow_depth, "required")) {
        requirements[["snd"]] <- list("snd")
    }
    requirements
}

# Expand recipe capabilities to the exact ESGF variables worth querying and
# extracting. This is deliberately internal: users still reason about the
# canonical variables returned by epw_morph_variables().
morpher__input_variables <- function(recipe) {
    requirements <- morpher__variable_requirements(recipe)
    required_inputs <- unique(unlist(requirements, recursive = TRUE, use.names = FALSE))
    optional <- epw_morph_variables(recipe, include_optional = TRUE)
    if (inherits(recipe, "epw_morph_recipe") &&
        recipe$backend %in% c("belcher", "belcher_absolute")) {
        if (!recipe$methods[["tdb"]] %in% c("auto", "combined")) {
            optional <- setdiff(optional, c("tasmax", "tasmin"))
        }
        if (!recipe$methods[["rh"]] %in% "combined") {
            optional <- setdiff(optional, c("hursmax", "hursmin"))
        }
        if (identical(recipe$options$snow_depth, "off")) {
            optional <- setdiff(optional, "snd")
        }
    }
    unique(c(required_inputs, optional))
}

# Test whether one set of available variables satisfies a canonical morphing
# requirement, preserving the declared alternative order for direct-data
# preference and deterministic diagnostics.
morpher__requirement_match <- function(available, alternatives) {
    available <- unique(as.character(available))
    for (alternative in alternatives) {
        alternative <- as.character(alternative)
        if (all(alternative %in% available)) {
            return(alternative)
        }
    }
    character()
}

#' EPW morphing recipe
#'
#' @param name Recipe name. Defaults to `"belcher"`.
#' @param backend Backend name. Ad hoc recipes default to `name`; registered
#'   recipes use the backend declared by their specification.
#' @param methods Optional named character vector overriding morphing methods for
#'        backend steps.
#' @param profile Built-in Belcher compatibility profile. `NULL` selects
#'   `"enhanced"`; old serialized recipes are reconstructed explicitly as
#'   `"legacy"`.
#' @param options Optional named backend option list. Belcher options are
#'   usually created by [belcher_options()].
#' @param policy Optional registered complete-recipe execution policy,
#'   `"paper_faithful"` or `"harmonized"`. Registered recipes select their
#'   declared default when `NULL`; ad hoc backend recipes do not accept it.
#' @param version Optional persisted registered-recipe definition version.
#'   Normally leave this `NULL`; resumed workflows use it to reject an
#'   incompatible catalog definition.
#' @param spec Optional registered complete-recipe identifier. This allows a
#'   stable catalog definition to be retained when `name` is a user-facing
#'   alias such as `"daily_temperature"`.
#'
#' @return A recipe list.
#' @export
epw_morph_recipe <- function(name = "belcher", backend = NULL, methods = NULL,
                             profile = NULL, options = NULL, policy = NULL,
                             version = NULL, spec = NULL) {
    checkmate::assert_string(name, min.chars = 1L)
    checkmate::assert_string(backend, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_string(policy, min.chars = 1L, null.ok = TRUE)
    checkmate::assert_count(version, positive = TRUE, null.ok = TRUE)
    checkmate::assert_string(spec, min.chars = 1L, null.ok = TRUE)
    name <- tolower(name)
    if (!is.null(policy)) {
        policy <- tolower(policy)
    }

    # Registered complete recipes resolve stable backend/profile identifiers
    # from the selected scientific policy. Ad hoc backend recipes retain the
    # historical name-to-backend default.
    spec_name <- if (is.null(spec)) name else tolower(spec)
    resolved <- recipe__resolve(
        spec_name,
        policy = policy,
        version = version
    )
    if (!is.null(spec) && is.null(resolved)) {
        cli::cli_abort(
            "Unknown registered future-weather recipe specification: {.val {spec_name}}."
        )
    }
    recipe_spec <- NULL
    recipe_version <- NULL
    if (is.null(resolved)) {
        if (is.null(backend)) {
            backend <- name
        }
        if (!is.null(policy)) {
            cli::cli_abort(
                "Execution {.arg policy} is available only for a registered future-weather recipe."
            )
        }
        if (!is.null(version)) {
            cli::cli_abort(
                "Recipe definition {.arg version} is available only for a registered future-weather recipe."
            )
        }
        policy <- NULL
    } else {
        recipe_spec <- resolved$spec@name
        recipe_version <- resolved$spec@version
        policy <- resolved$policy
        if (is.null(backend)) {
            backend <- resolved$spec@backend
        } else if (!identical(tolower(backend), resolved$spec@backend)) {
            cli::cli_abort(
                "Registered recipe {.val {name}} uses backend {.val {resolved$spec@backend}}, not {.val {tolower(backend)}}."
            )
        }
        if (!is.null(profile) &&
            !identical(tolower(profile), resolved$profile)) {
            cli::cli_abort(
                "Recipe policy {.val {policy}} requires backend profile {.val {resolved$profile}}."
            )
        }
        profile <- resolved$profile
    }

    backend <- tolower(backend)
    backend_spec <- epw_morph_backend(backend)

    is_belcher <- backend %in% c("belcher", "belcher_absolute")
    is_daily_temperature <- backend %in% c(
        "daily_temperature",
        "daily_temperature_btws"
    )
    is_eames_temperature <- identical(
        backend,
        "eames_monthly_temperature"
    )
    is_ek_temperature <- identical(backend, "ek_daily_temperature")
    is_arima_temperature <- identical(backend, "arima_temperature")
    is_sobie_curry <- identical(backend, "sobie_curry_daily")
    if (is_belcher) {
        if (is.null(profile)) {
            profile <- "enhanced"
        }
        checkmate::assert_choice(profile, EPW_MORPH_BELCHER_PROFILES)
        profile <- tolower(profile)
        base_methods <- morpher__belcher_profile_methods(backend_spec, profile)
        if (!is.null(methods)) {
            checkmate::assert_character(methods, any.missing = FALSE, names = "named")
            methods <- unlist(utils::modifyList(as.list(base_methods), as.list(methods)), use.names = TRUE)
        } else {
            methods <- base_methods
        }
        options <- morpher__belcher_resolve_options(profile, options)
    } else if (is_daily_temperature) {
        if (!is.null(profile) && !identical(profile, "default")) {
            cli::cli_abort(
                "Daily temperature recipes only support {.val default} profile metadata."
            )
        }
        profile <- "default"
        options <- daily__temperature_backend_options(options)
    } else if (is_eames_temperature) {
        if (!is.null(profile) && !identical(profile, "default")) {
            cli::cli_abort(
                "Eames monthly temperature recipes only support {.val default} profile metadata."
            )
        }
        profile <- "default"
        options <- eames__monthly_temperature_options(options)
    } else if (is_ek_temperature) {
        if (!is.null(profile) && !identical(profile, "default")) {
            cli::cli_abort(
                "Ek daily temperature recipes only support {.val default} profile metadata."
            )
        }
        profile <- "default"
        options <- ek__daily_temperature_options(options)
    } else if (is_arima_temperature) {
        if (!is.null(profile) && !identical(profile, "default")) {
            cli::cli_abort(
                "Arima temperature recipes only support {.val default} profile metadata."
            )
        }
        profile <- "default"
        options <- arima__temperature_options(options)
    } else if (is_sobie_curry) {
        if (!is.null(profile) && !identical(profile, "default")) {
            cli::cli_abort(
                "Sobie-Curry recipes only support {.val default} profile metadata."
            )
        }
        profile <- "default"
        options <- sobie__backend_options(options)
    } else {
        if (!is.null(profile) && !identical(profile, "default")) {
            cli::cli_abort("Custom EPW morphing backends only support {.val default} profile metadata.")
        }
        profile <- "default"
        if (is.null(options)) {
            options <- list()
        } else if (!is.list(options)) {
            cli::cli_abort("Custom backend `options` must be a named list.")
        }
    }

    methods <- morpher__recipe_methods(methods, backend_spec)
    rules <- backend_spec$rules_with_methods(methods)
    pipeline <- backend_spec$component_pipeline()
    if (is_belcher && identical(options$snow_depth, "required")) {
        rules[step == "snow_depth", required := TRUE]
    }

    structure(
        list(
            name = name,
            backend = backend,
            profile = profile,
            options = options,
            methods = methods,
            rules = rules,
            recipe_spec = recipe_spec,
            recipe_version = recipe_version,
            policy = policy,
            components = if (!is.null(resolved)) {
                resolved$spec@components
            } else if (is.null(pipeline)) {
                NULL
            } else {
                pipeline__records(pipeline)
            }
        ),
        class = "epw_morph_recipe"
    )
}

#' EPW morphing periods
#'
#' @param ... Named integer year vectors.
#'
#' @return A data.table with columns `period` and `year`.
#' @export
epw_morph_periods <- function(...) {
    periods <- list(...)
    if (!length(periods)) {
        cli::cli_abort("At least one named period must be supplied.")
    }
    nms <- names(periods)
    if (is.null(nms) || any(!nzchar(nms))) {
        cli::cli_abort("All EPW morphing periods must be named.")
    }

    rows <- lapply(seq_along(periods), function(i) {
        years <- periods[[i]]
        checkmate::assert_integerish(years, lower = 1900, any.missing = FALSE, min.len = 1L, unique = TRUE)
        data.table::data.table(period = nms[[i]], year = as.integer(sort(years)))
    })
    data.table::rbindlist(rows)
}

morpher__recipe_rules <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    data.table::as.data.table(recipe$rules)
}

morpher__recipe_methods <- function(methods = NULL, backend = epw_morph_backend("belcher")) {
    if (!inherits(backend, "EpwMorphBackend")) {
        cli::cli_abort("`backend` must be an {.cls EpwMorphBackend} object.")
    }
    backend$validate_methods(methods)
}

# Resolve the stable complete-recipe definition recorded with a configured
# recipe. Ad hoc backend recipes deliberately return NULL.
morpher__recipe_spec <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    if (is.null(recipe$recipe_spec)) {
        return(NULL)
    }
    recipe__get(
        recipe$recipe_spec,
        version = recipe$recipe_version
    )
}

# Report whether one semantic input role is required by a registered recipe.
# Ad hoc backends predate role-addressable contracts and can require only the
# historical model reference represented by their backend flag.
morpher__recipe_requires_role <- function(recipe, role) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    checkmate::assert_choice(role, WEATHER_INPUT_ROLES)
    spec <- morpher__recipe_spec(recipe)
    if (!is.null(spec)) {
        return(role %in% names(spec@required_inputs))
    }
    identical(role, "model_historical") &&
        isTRUE(epw_morph_backend(recipe$backend)$requires_reference)
}

# Report whether one semantic input role is accepted by a registered recipe.
# Required roles are necessarily accepted; optional roles remain explicit.
morpher__recipe_accepts_role <- function(recipe, role) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    checkmate::assert_choice(role, WEATHER_INPUT_ROLES)
    spec <- morpher__recipe_spec(recipe)
    if (!is.null(spec)) {
        return(role %in% c(
            names(spec@required_inputs),
            names(spec@optional_inputs)
        ))
    }
    identical(role, "model_historical") &&
        isTRUE(epw_morph_backend(recipe$backend)$accepts_reference)
}

morpher__recipe_requires_reference <- function(recipe) {
    morpher__recipe_requires_role(recipe, "model_historical")
}

# Report whether a recipe can consume external climate reference data while
# still distinguishing optional-reference backends from required ones.
morpher__recipe_accepts_reference <- function(recipe) {
    morpher__recipe_accepts_role(recipe, "model_historical")
}

# Keep observations separate from historical model output throughout workflow
# validation so methods needing all four input roles cannot accept one in place
# of the other.
morpher__recipe_requires_observed_reference <- function(recipe) {
    morpher__recipe_requires_role(recipe, "observed_reference")
}

# Report whether a recipe can consume a multi-year observed daily reference.
morpher__recipe_accepts_observed_reference <- function(recipe) {
    morpher__recipe_accepts_role(recipe, "observed_reference")
}

# Return the component-declared CMIP frequency without duplicating that
# constraint on the backend or staged workflow.
morpher__recipe_required_frequency <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    recipe_spec <- morpher__recipe_spec(recipe)
    if (!is.null(recipe_spec)) {
        choices <- recipe__frequency_choices(recipe_spec)
    } else {
        spec <- pipeline__from_records(recipe$components)
        if (is.null(spec)) {
            return(NULL)
        }
        choices <- pipeline__frequency_choices(spec)
    }
    if (length(choices) > 1L) {
        cli::cli_abort(
            "The current shift workflow requires one CMIP frequency; recipe choices are {.val {choices}}."
        )
    }
    choices
}

# Build a structural diagnostic when extracted or summarized climate data do not
# match a backend's declared CMIP frequency.
morpher__frequency_diagnostic <- function(
    recipe, frequency, stage, plan_id = NA_character_,
    summary_id = NA_character_
) {
    required <- morpher__recipe_required_frequency(recipe)
    if (is.null(required)) {
        return(morpher__empty_diagnostics())
    }
    actual <- unique(tolower(as.character(frequency)))
    actual <- actual[!is.na(actual) & nzchar(actual)]
    if (identical(actual, required)) {
        return(morpher__empty_diagnostics())
    }
    shown <- if (length(actual)) paste(actual, collapse = ", ") else "<missing>"
    morpher__diagnostic(
        stage = stage,
        severity = "error",
        code = "unsupported_climate_frequency",
        message = sprintf(
            "Backend %s requires CMIP frequency %s; found %s.",
            recipe$backend,
            required,
            shown
        ),
        plan_id = plan_id,
        summary_id = summary_id,
        action = sprintf(
            "Extract climate data with frequency %s before morphing.",
            required
        )
    )
}

morpher__recipe_method_overrides <- function(recipe) {
    if (!inherits(recipe, "epw_morph_recipe")) {
        cli::cli_abort("`recipe` must be created by {.fn epw_morph_recipe}.")
    }
    backend <- epw_morph_backend(recipe$backend)
    methods <- recipe$methods
    if (is.null(methods)) {
        return(NULL)
    }
    defaults <- if (recipe$backend %in% c("belcher", "belcher_absolute")) {
        morpher__belcher_profile_methods(backend, recipe$profile)
    } else {
        backend$methods()
    }
    overrides <- methods[names(methods) %in% names(defaults) & methods != defaults[names(methods)]]
    if (!length(overrides)) NULL else overrides
}
# }}}
