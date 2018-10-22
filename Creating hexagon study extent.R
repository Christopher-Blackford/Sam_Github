#Creating hexagon study extent

###Loading packages
library(rgdal)
library(rgeos)
library(sp)

#Loading EEZ
EEZ <- readOGR("./shapefiles/eez_project", "eez_project")
#EEZ@proj4string = CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

Poly_coords <- matrix(c(-69.14795, 44.02442, 
                        -68.106, 40.426,
                        -62.457, 38.505, 
                        -52.631, 41.334, 
                        -47.000, 46.000, 
                        -46.31836, 50.62507, 
                        -48.60352, 54.74999, 
                        -53.65723, 58.63122,
                        -63.32520, 59.04055,
                        -75.353, 44.636,
                        -69.14795, 44.02442), ncol=2, byrow=TRUE)

#Going from list of coordinates to spatial polygon
Study_area <- Polygon(coords = Poly_coords)
Study_area <- list(Study_area)
Study_area <- Polygons(Study_area, ID="1")
Study_area <- list(Study_area)
Study_area <- SpatialPolygons(Study_area, proj4string = CRS("+proj=longlat +datum=WGS84"))

#Projecting shapefile to albers equal area conic
Study_area <- spTransform(Study_area, CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
#Intersecting with EEZ
Study_area <- gIntersection(Study_area, EEZ, byid = FALSE, drop_lower_td = TRUE) #This works, but you'll have to choose a shapefile that includes islands and doesn't cut-off at rivers 

#Adding a dummy dataframe to spatial polygon to make "SpatialPolygonsDataFrame" i.e. file that can be exported to shapefile
ID <- row.names(Study_area) 
ID <- as.data.frame(ID)
Study_area <- SpatialPolygonsDataFrame(Study_area, data = ID)
plot(Study_area)

writeOGR(Study_area, dsn = "./output/shapefiles", layer = "Study_Area", 
         driver = "ESRI Shapefile", verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)


###Creating the hexagon layer
Study_area_hex <- Study_area

#0.25x0.25 degrees is approx. 20 horizontal X 28 vertical km = 560 km squared
hex_area_km <- 560
hex_area <- hex_area_km*10^6
dist_between_centroids <- sqrt(2*hex_area/sqrt(3)) #This formula converts between interior hexagon area which is what we are interested in and distance between hexagon centroids which R uses to define hexagon layer

Study_area_hex = gBuffer(Study_area_hex, width = 0)
HexPts <- spsample(Study_area_hex, type="hexagonal", cellsize = dist_between_centroids, offset = c(0, 0))
Study_area_hex <- HexPoints2SpatialPolygons(HexPts)
plot(Study_area_hex)

#Adding a dummy dataframe to spatial polygon to make "SpatialPolygonsDataFrame" i.e. file that can be exported to shapefile
row.names(Study_area_hex) <- as.character(1:length(Study_area_hex))
Poly_ID <- 1:length(Study_area_hex)

Study_area_hex <- SpatialPolygonsDataFrame(Study_area_hex, as.data.frame(Poly_ID))

plot(Study_area_hex)

writeOGR(Study_area_hex, dsn = "./output/shapefiles", layer = "Study_Area_hex", 
         driver = "ESRI Shapefile", verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)