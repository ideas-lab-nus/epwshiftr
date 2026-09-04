# Shared solar mathematical kernels {{{

# Convert angular degrees to radians for geographic and solar calculations.
solar__radians <- function(degree) {
    degree * pi / 180
}

# Evaluate Spencer's Fourier-series solar declination from an annual day angle.
solar__spencer_declination <- function(day_angle) {
    0.006918 -
        0.399912 * cos(day_angle) +
        0.070257 * sin(day_angle) -
        0.006758 * cos(2 * day_angle) +
        0.000907 * sin(2 * day_angle) -
        0.002697 * cos(3 * day_angle) +
        0.001480 * sin(3 * day_angle)
}

# Evaluate Spencer's equation of time in minutes from an annual day angle.
solar__spencer_equation_of_time <- function(day_angle) {
    229.18 * (
        0.000075 +
            0.001868 * cos(day_angle) -
            0.032077 * sin(day_angle) -
            0.014615 * cos(2 * day_angle) -
            0.040849 * sin(2 * day_angle)
    )
}

# Calculate cosine of solar zenith from latitude, declination, and hour angle,
# all expressed in radians, without applying a daylight or horizon policy.
solar__cos_zenith <- function(latitude, declination, hour_angle) {
    sin(latitude) * sin(declination) +
        cos(latitude) * cos(declination) * cos(hour_angle)
}

# }}}
