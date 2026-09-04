#' @include adapter-direct-model-calendar.R epw-file.R epw-morph-context.R epw-physics.R
NULL

# The direct-model EPW physical boundary requires the climate families needed
# to construct the principal EnergyPlus weather drivers for every future year.
DIRECT_EPW_CORE_VARIABLES <- c("tas", "ps", "rsds", "rsdsdiff")
DIRECT_EPW_HUMIDITY_VARIABLES <- c("hurs", "huss")
DIRECT_EPW_WIND_VARIABLES <- c("sfcWind", "uas", "vas")
DIRECT_EPW_OPTIONAL_VARIABLES <- "rlds"
DIRECT_EPW_SUPPORTED_VARIABLES <- c(
    DIRECT_EPW_CORE_VARIABLES,
    DIRECT_EPW_HUMIDITY_VARIABLES,
    DIRECT_EPW_WIND_VARIABLES,
    DIRECT_EPW_OPTIONAL_VARIABLES
)

# Modified fields are declared once so diagnostics and later output components
# can distinguish constructed values from fields inherited from the EPW template.
DIRECT_EPW_CONSTRUCTED_FIELDS <- c(
    "dry_bulb_temperature",
    "dew_point_temperature",
    "relative_humidity",
    "atmospheric_pressure",
    "global_horizontal_radiation",
    "direct_normal_radiation",
    "diffuse_horizontal_radiation",
    "wind_speed"
)

# Validate one physically closed future year without depending on a complete
# recipe or the final WeatherSequenceResult output wrapper.
direct_epw__member_error <- function(self) {
    if (length(self@sequence_id) != 1L ||
        is.na(self@sequence_id) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", self@sequence_id)) {
        return("`sequence_id` contains unsupported characters.")
    }
    if (length(self@weather_year) != 1L ||
        is.na(self@weather_year) ||
        self@weather_year < 1L) {
        return("`weather_year` must be one positive integer.")
    }
    if (length(self@source_calendar) != 1L ||
        is.na(self@source_calendar) ||
        !self@source_calendar %in% CF_TIME_CALENDARS) {
        return("`source_calendar` must identify one supported CF calendar.")
    }
    if (!is.data.frame(self@data) ||
        nrow(self@data) != HOURMAP_TARGET_HOURS) {
        return("`data` must contain exactly 8760 EPW weather rows.")
    }
    missing <- setdiff(EPW_FILE_COLUMNS, names(self@data))
    if (length(missing)) {
        return(sprintf(
            "`data` is missing EPW field(s): %s.",
            paste(sprintf("`%s`", missing), collapse = ", ")
        ))
    }
    years <- unique(as.integer(self@data[["year"]]))
    if (!identical(years, self@weather_year)) {
        return("Every physically closed row must match `weather_year`.")
    }
    if (!is.data.frame(self@diagnostics) || nrow(self@diagnostics) != 1L) {
        return("`diagnostics` must contain one row for the closed weather year.")
    }
    if (length(self@provenance) &&
        (is.null(names(self@provenance)) ||
            any(!nzchar(names(self@provenance))) ||
            anyDuplicated(names(self@provenance)))) {
        return("`provenance` must be a uniquely named list.")
    }
    NULL
}

# EpwHourlyWeatherMember carries one baseline-shaped future EPW year after all
# dependent thermodynamic, wind, and shortwave fields have been closed.
EpwHourlyWeatherMember <- S7::new_class(
    "EpwHourlyWeatherMember",
    properties = list(
        sequence_id = S7::new_property(S7::class_character),
        weather_year = S7::new_property(S7::class_integer),
        source_calendar = S7::new_property(S7::class_character),
        data = S7::new_property(S7::class_any),
        diagnostics = S7::new_property(S7::class_any),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = direct_epw__member_error
)

# Validate the ordered collection of closed years before an output component
# converts it into the package's public multi-year result contract.
direct_epw__sequence_error <- function(self) {
    if (!length(self@members) ||
        !all(vapply(
            self@members,
            S7::S7_inherits,
            logical(1L),
            class = EpwHourlyWeatherMember
        ))) {
        return("`members` must contain EpwHourlyWeatherMember objects.")
    }
    if (!identical(self@target_calendar, "epw_365_day")) {
        return("`target_calendar` must be `epw_365_day`.")
    }
    if (!length(self@constructed_fields) ||
        anyNA(self@constructed_fields) ||
        any(!self@constructed_fields %in% EPW_FILE_COLUMNS) ||
        anyDuplicated(self@constructed_fields)) {
        return(
            "`constructed_fields` must contain unique supported EPW fields."
        )
    }
    years <- vapply(
        self@members,
        function(member) member@weather_year,
        integer(1L)
    )
    if (anyDuplicated(years) || !identical(years, sort(years))) {
        return("Closed EPW weather members must use unique ascending years.")
    }
    sequence_ids <- vapply(
        self@members,
        function(member) member@sequence_id,
        character(1L)
    )
    if (length(unique(sequence_ids)) != 1L) {
        return("Closed EPW weather members must share one `sequence_id`.")
    }
    if (length(self@provenance) &&
        (is.null(names(self@provenance)) ||
            any(!nzchar(names(self@provenance))) ||
            anyDuplicated(names(self@provenance)))) {
        return("`provenance` must be a uniquely named list.")
    }
    NULL
}

# EpwHourlyWeatherSequence is the typed output of the physics stage and the
# direct input expected by the later multi-year EPW result writer.
EpwHourlyWeatherSequence <- S7::new_class(
    "EpwHourlyWeatherSequence",
    properties = list(
        members = S7::new_property(S7::class_list),
        target_calendar = S7::new_property(S7::class_character),
        constructed_fields = S7::new_property(S7::class_character),
        provenance = S7::new_property(S7::class_list, default = list())
    ),
    validator = direct_epw__sequence_error
)

# Collect one mapped member into unique variable tables while retaining the
# row-level mapping metadata for provenance and validation.
direct_epw__variables <- function(member) {
    variables <- unlist(lapply(
        member@series,
        function(series) series@variables
    ), use.names = FALSE)
    if (anyDuplicated(variables)) {
        cli::cli_abort(
            "Weather year {member@weather_year} contains duplicate mapped variable groups: {.val {unique(variables[duplicated(variables)])}}."
        )
    }
    tables <- lapply(variables, function(variable) {
        containing <- Filter(
            function(series) variable %in% series@variables,
            member@series
        )
        rows <- containing[[1L]]@data[
            containing[[1L]]@data[["variable_id"]] == variable,
            ,
            drop = FALSE
        ]
        rows[order(rows[["epw_row"]]), , drop = FALSE]
    })
    names(tables) <- variables
    tables
}

# Resolve the mutually exclusive humidity and wind input paths before any
# values are converted so ambiguity cannot be hidden by downstream clipping.
direct_epw__variable_contract <- function(variables, weather_year) {
    present <- names(variables)
    unsupported <- setdiff(present, DIRECT_EPW_SUPPORTED_VARIABLES)
    if (length(unsupported)) {
        cli::cli_abort(
            "Weather year {weather_year} contains unsupported EPW physical variable(s): {.val {unsupported}}."
        )
    }
    missing <- setdiff(DIRECT_EPW_CORE_VARIABLES, present)
    if (length(missing)) {
        cli::cli_abort(
            "Weather year {weather_year} is missing required EPW physical variable(s): {.val {missing}}."
        )
    }

    humidity <- intersect(DIRECT_EPW_HUMIDITY_VARIABLES, present)
    if (length(humidity) != 1L) {
        cli::cli_abort(
            "Weather year {weather_year} must provide exactly one humidity path: `hurs` or `huss`."
        )
    }
    has_speed <- "sfcWind" %in% present
    vector_parts <- intersect(c("uas", "vas"), present)
    if (length(vector_parts) == 1L) {
        cli::cli_abort(
            "Weather year {weather_year} must provide `uas` and `vas` together."
        )
    }
    has_vector <- length(vector_parts) == 2L
    if (identical(has_speed, has_vector)) {
        cli::cli_abort(
            "Weather year {weather_year} must provide exactly one wind path: `sfcWind` or paired `uas` and `vas`."
        )
    }
    list(
        humidity = humidity[[1L]],
        wind = if (has_speed) "sfcWind" else "uas_vas",
        has_rlds = "rlds" %in% present
    )
}

# Convert one mapped variable using a narrow unit schema chosen for the EPW
# fields constructed by this component.
direct_epw__values <- function(variables, variable) {
    rows <- variables[[variable]]
    if (is.null(rows)) {
        cli::cli_abort("Mapped EPW climate variable {.val {variable}} is missing.")
    }
    raw_units <- unique(as.character(rows[["units"]]))
    units <- unique(vapply(
        raw_units,
        morpher__unit_alias,
        character(1L)
    ))
    if (length(units) != 1L) {
        cli::cli_abort(
            "Mapped EPW climate variable {.val {variable}} must use one unit."
        )
    }
    unit <- units[[1L]]
    allowed <- switch(
        variable,
        tas = c("K", "degC"),
        ps = c("Pa", "hPa"),
        hurs = "%",
        huss = c("1", "kg/kg"),
        sfcWind = "m/s",
        uas = "m/s",
        vas = "m/s",
        rsds = "W/m^2",
        rsdsdiff = "W/m^2",
        rlds = "W/m^2"
    )
    if (is.na(unit) || !unit %in% allowed) {
        cli::cli_abort(
            "Mapped EPW climate variable {.val {variable}} uses unsupported unit {.val {unit}}; expected {.val {allowed}}."
        )
    }
    target_unit <- switch(
        variable,
        tas = epw_file_unit("dry_bulb_temperature"),
        ps = epw_file_unit("atmospheric_pressure"),
        hurs = epw_file_unit("relative_humidity"),
        huss = unit,
        sfcWind = epw_file_unit("wind_speed"),
        uas = epw_file_unit("wind_speed"),
        vas = epw_file_unit("wind_speed"),
        rsds = epw_file_unit("global_horizontal_radiation"),
        rsdsdiff = epw_file_unit("diffuse_horizontal_radiation"),
        rlds = epw_file_unit(
            "horizontal_infrared_radiation_intensity_from_sky"
        )
    )
    converted <- morpher__convert_value_checked(
        rows[["value"]],
        unit,
        target_unit
    )
    if (!isTRUE(converted$ok)) {
        cli::cli_abort(converted$message)
    }
    converted$value
}

# Compute solar geometry on a fixed non-leap surrogate year so a future leap
# year cannot shift March-through-December EPW rows by one astronomical day.
direct_epw__solar_geometry <- function(template, epw) {
    latitude <- morpher__epw_location_numeric(
        epw,
        c("latitude", "lat", "N2_latitude")
    )
    longitude <- morpher__epw_location_numeric(
        epw,
        c("longitude", "lon", "N3_longitude")
    )
    timezone <- morpher__epw_location_numeric(
        epw,
        c("time_zone", "timezone", "N4_time_zone"),
        default = 0
    )
    geometry_input <- data.table::data.table(
        year = rep.int(2001L, nrow(template)),
        month = as.integer(template[["month"]]),
        day = as.integer(template[["day"]]),
        hour = as.integer(template[["hour"]])
    )
    solar__epw_interval_geometry(
        geometry_input,
        latitude,
        longitude,
        timezone
    )
}

# Construct one future EPW weather year and retain every field outside the
# declared climate families exactly as stored in the baseline template. The
# adapter supplies source-specific candidates; the shared physical layer owns
# every derivation, bound, and closure operation.
direct_epw__member <- function(member, epw, template, geometry) {
    variables <- direct_epw__variables(member)
    contract <- direct_epw__variable_contract(
        variables,
        member@weather_year
    )
    physical_template <- data.table::as.data.table(data.table::copy(template))
    data.table::set(
        physical_template,
        j = "year",
        value = rep.int(member@weather_year, nrow(physical_template))
    )
    fields <- list(
        dry_bulb_temperature = direct_epw__values(variables, "tas"),
        atmospheric_pressure = direct_epw__values(variables, "ps")
    )
    humidity <- if (identical(contract$humidity, "hurs")) {
        list(relative_humidity = direct_epw__values(variables, "hurs"))
    } else {
        list(target_specific_humidity =
            direct_epw__values(variables, "huss"))
    }
    wind <- if (identical(contract$wind, "sfcWind")) {
        list(speed = direct_epw__values(variables, "sfcWind"))
    } else {
        list(
            eastward = direct_epw__values(variables, "uas"),
            northward = direct_epw__values(variables, "vas")
        )
    }
    constructed_fields <- DIRECT_EPW_CONSTRUCTED_FIELDS
    if (identical(contract$wind, "uas_vas")) {
        constructed_fields <- c(constructed_fields, "wind_direction")
    }
    if (contract$has_rlds) {
        fields$horizontal_infrared_radiation_intensity_from_sky <-
            direct_epw__values(variables, "rlds")
        constructed_fields <- c(
            constructed_fields,
            "horizontal_infrared_radiation_intensity_from_sky"
        )
    }
    physical <- epwphys__apply(
        EpwPhysicalRequest(
            template = physical_template,
            fields = fields,
            humidity = humidity,
            wind = wind,
            shortwave = list(
                global_horizontal = direct_epw__values(variables, "rsds"),
                diffuse_horizontal =
                    direct_epw__values(variables, "rsdsdiff")
            ),
            geometry = geometry,
            provenance = list(
                source_calendar = member@source_calendar,
                source_variables = names(variables)
            )
        ),
        epwphys__policy("absolute_model_fields")
    )
    weather <- physical@weather
    corrections <- physical@corrections
    wind_state <- physical@state$wind
    if ("datetime" %in% names(weather)) {
        data.table::set(
            weather,
            j = "datetime",
            value = epw_file_datetime(
                weather[["year"]],
                weather[["month"]],
                weather[["day"]],
                weather[["hour"]]
            )
        )
        data.table::setcolorder(
            weather,
            c("datetime", setdiff(names(weather), "datetime"))
        )
    }

    diagnostics <- data.frame(
        weather_year = member@weather_year,
        source_calendar = member@source_calendar,
        humidity_source = contract$humidity,
        wind_source = contract$wind,
        wind_direction_policy = wind_state$direction_policy,
        temperature_clipped = corrections$temperature_clipped,
        pressure_clipped = corrections$pressure_clipped,
        humidity_saturation_clipped =
            corrections$humidity_saturation_clipped,
        specific_humidity_clipped =
            corrections$specific_humidity_clipped,
        dew_point_clipped = corrections$dew_point_clipped,
        wind_speed_clipped = corrections$wind_speed_clipped,
        radiation_night_values_zeroed =
            corrections$radiation_night_values_zeroed,
        radiation_negative_global_clipped =
            corrections$radiation_negative_global_clipped,
        radiation_negative_diffuse_clipped =
            corrections$radiation_negative_diffuse_clipped,
        radiation_diffuse_above_global_clipped =
            corrections$radiation_diffuse_above_global_clipped,
        radiation_excess_beam_reallocated =
            corrections$radiation_excess_beam_reallocated,
        radiation_maximum_closure_error =
            corrections$radiation_maximum_closure_error,
        infrared_negative_clipped = corrections$infrared_negative_clipped,
        stringsAsFactors = FALSE
    )
    EpwHourlyWeatherMember(
        sequence_id = member@sequence_id,
        weather_year = member@weather_year,
        source_calendar = member@source_calendar,
        data = weather[],
        diagnostics = diagnostics,
        provenance = list(
            method = "epw_hourly_physical_closure",
            source = member@provenance,
            source_variables = names(variables),
            humidity_source = contract$humidity,
            wind_source = contract$wind,
            wind_direction_policy = wind_state$direction_policy,
            physical_policy = physical@policy@name,
            solar_calendar = "fixed_365_day",
            shortwave_closure = "ghi_equals_dhi_plus_dni_projection",
            constructed_fields = constructed_fields,
            inherited_fields = setdiff(
                EPW_FILE_COLUMNS,
                c("year", constructed_fields)
            )
        )
    )
}

# Close every mapped climate member against one shared EPW template while
# allowing unrelated recipe options to remain owned by their declared stages.
direct_epw__apply <- function(data, inputs, context, options) {
    if (!S7::S7_inherits(data, MappedHourlyClimateSequence)) {
        cli::cli_abort(
            "`epw_hourly_physical_closure` requires a MappedHourlyClimateSequence object."
        )
    }
    checkmate::assert_list(options, names = "unique")
    template_input <- weather__get_input(inputs, "weather_template")
    if (!S7::S7_inherits(template_input, WeatherInput) ||
        !identical(template_input@representation, "epw") ||
        !inherits(template_input@source, "EpwFile")) {
        cli::cli_abort(
            "Role `weather_template` must contain an EPW WeatherInput."
        )
    }
    epw <- template_input@source
    hourmap__target_grid(epw)
    template <- data.table::as.data.table(data.table::copy(epw$data()))
    geometry <- direct_epw__solar_geometry(template, epw)
    members <- lapply(
        data@members,
        direct_epw__member,
        epw = epw,
        template = template,
        geometry = geometry
    )
    EpwHourlyWeatherSequence(
        members = members,
        target_calendar = "epw_365_day",
        constructed_fields = unique(unlist(lapply(
            members,
            function(member) member@provenance$constructed_fields
        ), use.names = FALSE)),
        provenance = list(
            method = "epw_hourly_physical_closure",
            physical_policy = "absolute_model_fields",
            source = data@provenance,
            target_calendar = "epw_365_day",
            thermodynamic_closure = "humidity_dew_point_temperature_pressure",
            solar_closure = "ghi_dhi_dni_fixed_365_day_geometry",
            unchanged_field_policy = "inherit_epw_template",
            member_count = length(members)
        )
    )
}

# Describe the shared physical closure independently of the signal method or
# complete future-weather recipe that supplies the mapped climate sequence.
direct_epw__component <- function() {
    component__spec(
        name = "epw_hourly_physical_closure",
        stage = "physics",
        label = "EPW hourly physical closure",
        required_inputs = list(
            weather_template = component__input_requirement(
                "weather_template",
                representations = "epw",
                frequencies = "hour",
                calendars = "gregorian"
            )
        ),
        input_kinds = "epw_hourly_climate_sequence",
        output_kinds = "epw_hourly_weather_sequence",
        scopes = "multivariate",
        stochastic = FALSE,
        operations = list(apply = direct_epw__apply),
        metadata = list(
            algorithm = "epw_hourly_physical_closure",
            required_variables = DIRECT_EPW_CORE_VARIABLES,
            humidity_alternatives = DIRECT_EPW_HUMIDITY_VARIABLES,
            wind_alternatives = list(
                scalar = "sfcWind",
                vector = c("uas", "vas")
            ),
            optional_variables = DIRECT_EPW_OPTIONAL_VARIABLES,
            physical_policies = "absolute_model_fields",
            target_calendar = "epw_365_day",
            shortwave_closure = "ghi_equals_dhi_plus_dni_projection",
            unchanged_field_policy = "inherit_epw_template",
            output_contract = "epw_hourly_weather_sequence"
        )
    )
}

# Register the method-neutral physical boundary once so later output and recipe
# components can resolve it through the shared component registry.
direct_epw__register_component <- function() {
    component__register_builtin(direct_epw__component())
    invisible(NULL)
}
