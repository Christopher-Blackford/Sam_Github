#Hexagon 2

###Loading packages
library(sf)


OBIS_pts <- matrix(c(-69.14795, 44.02442, -67.71973, 41.45920, 
-61.52344, 40.01079, -53.17383, 41.57436, -47.37305, 46.31658, 
-46.31836, 50.62507, -48.60352, 54.74999, -53.65723, 58.63122, 
-63.32520, 59.04055, -73.87207, 46.76997, -69.14795, 44.02442), ncol=2, byrow=TRUE)

OBIS_pts = list(OBIS_pts)

OBIS_poly <- st_polygon(OBIS_pts)
class(OBIS_poly)
plot(OBIS_poly)

new = st_transform(OBIS_poly, "+proj=eqc")


?st_as_sf



str(OBIS_poly)