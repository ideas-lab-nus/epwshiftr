# Shared solar mathematical kernels
# Convert angular degrees to radians for geographic and solar calculations.
# solar__radians {{{
solar__radians <- function(degree) {
    degree * pi / 180
}
# }}}

# Evaluate Spencer's Fourier-series solar declination from an annual day angle.
# solar__spencer_declination {{{
solar__spencer_declination <- function(day_angle, terms = NULL) {
    # Standalone callers compute only the terms required by this formula.
    if (is.null(terms)) {
        terms <- solar__spencer_terms(day_angle)
    }
    0.006918 -
        0.399912 * terms$cos_one +
        0.070257 * terms$sin_one -
        0.006758 * terms$cos_two +
        0.000907 * terms$sin_two -
        0.002697 * cos(3 * day_angle) +
        0.001480 * sin(3 * day_angle)
}
# }}}

# Evaluate Spencer's equation of time in minutes from an annual day angle.
# solar__spencer_equation_of_time {{{
solar__spencer_equation_of_time <- function(day_angle, terms = NULL) {
    # Standalone callers compute only the terms required by this formula.
    if (is.null(terms)) {
        terms <- solar__spencer_terms(day_angle)
    }
    229.18 *
        (0.000075 +
            0.001868 * terms$cos_one -
            0.032077 * terms$sin_one -
            0.014615 * terms$cos_two -
            0.040849 * terms$sin_two)
}
# }}}

# Compute just the four shared trigonometric arrays. Preserve input matrix and
# name attributes and keep all values local to the requesting geometry call.
# solar__spencer_terms {{{
solar__spencer_terms <- function(day_angle) {
    list(
        cos_one = cos(day_angle),
        sin_one = sin(day_angle),
        cos_two = cos(2 * day_angle),
        sin_two = sin(2 * day_angle)
    )
}
# }}}

# Reuse trigonometric arrays while delegating scientific coefficients to the
# original kernels. Only EPW geometry requests the eccentricity calculation.
# solar__spencer_geometry {{{
solar__spencer_geometry <- function(day_angle, include_eccentricity = FALSE) {
    terms <- solar__spencer_terms(day_angle)
    result <- list(
        declination = solar__spencer_declination(day_angle, terms = terms),
        equation_of_time = solar__spencer_equation_of_time(
            day_angle,
            terms = terms
        )
    )
    if (include_eccentricity) {
        result$eccentricity <- 1.000110 +
            0.034221 * terms$cos_one +
            0.001280 * terms$sin_one +
            0.000719 * terms$cos_two +
            0.000077 * terms$sin_two
    }
    result
}
# }}}

# Calculate cosine of solar zenith from latitude, declination, and hour angle,
# all expressed in radians, without applying a daylight or horizon policy.
# solar__cos_zenith {{{
solar__cos_zenith <- function(latitude, declination, hour_angle) {
    sin(latitude) *
        sin(declination) +
        cos(latitude) * cos(declination) * cos(hour_angle)
}
# }}}

# Preserve the baseline diffuse fraction after a method changes GHI. Zero-GHI
# hours remain fully diffuse so no beam component is synthesized from darkness.
# radiation__preserved_diffuse {{{
radiation__preserved_diffuse <- function(data_epw, ghi) {
    baseline_ghi <- pmax(
        0,
        as.numeric(data_epw[["global_horizontal_radiation"]])
    )
    baseline_dhi <- pmax(
        0,
        as.numeric(data_epw[["diffuse_horizontal_radiation"]])
    )
    fraction <- ifelse(
        baseline_ghi > .Machine$double.eps,
        pmin(1, baseline_dhi / baseline_ghi),
        1
    )
    pmin(ghi, pmax(0, as.numeric(ghi) * fraction))
}
# }}}

# vim: fdm=marker :
