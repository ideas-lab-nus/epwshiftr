# Geographic helpers ----------------------------------------------------------

to_radian <- function(degree) {
    degree * pi / 180.0
}

# Ref: https://en.wikipedia.org/wiki/Geographical_distance#Tunnel_distance
tunnel_dist <- function(lat1, lon1, lat2, lon2) {
    earth_radius <- 6371.009

    lat1 <- to_radian(lat1)
    lon1 <- to_radian(lon1)

    lat2 <- to_radian(lat2)
    lon2 <- to_radian(lon2)

    delta_x <- cos(lat2) * cos(lon2) - cos(lat1) * cos(lon1)
    delta_y <- cos(lat2) * sin(lon2) - cos(lat1) * sin(lon1)
    delta_z <- sin(lat2) - sin(lat1)

    sqrt(delta_x ^ 2 + delta_y ^ 2 + delta_z ^ 2) * earth_radius
}
