#Clear workspace
rm(list=ls())

library(rgdal)
library(rgeos)
library(sp)

#Clipping to your study extent
EEZ <- readOGR("./shapefiles/Atlantic_eez", "Atlantic_eez")

#Loading Remi's grid where larvae were released
#grid <- readOGR("./shapefiles", "")
#NAD_projection <- proj4string(grid)

#Dissolve into one polygon since so you can change grid dimensions
#grid <- spTransform(grid, EEZ@proj4string)
#grid <- gUnaryUnion(grid)

#Intersecting - don't know why this works and ConPoly2 <- grid[EEZ,] doesn't
#ConPoly <- gIntersection(grid, EEZ, byid = FALSE, drop_lower_td = TRUE) #This works, but you'll have to choose a shapefile that includes islands and doesn't cut-off at rivers 


#Adding dataframe so you can create a shapefile of new study extent
#ConPoly_ID <- 1 
#ConPoly <- SpatialPolygonsDataFrame(ConPoly, as.data.frame(ConPoly_ID))

#writeOGR(ConPoly, dsn = "", layer = "EEZ_hexagons", 
 #        driver = "ESRI Shapefile", verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)

ConPoly <- EEZ

#Creating the hexagon layer
ConPoly_hex <- ConPoly

#0.25 degrees is approx. 20 horizontal X 28 vertical km = 560 km2
hex_area_km <- 560
hex_area <- hex_area_km*1000000
dist_between_centroids <- sqrt(2*hex_area/sqrt(3)) #This formula converts between interior hexagon area which is what we are interested in and distance between hexagon centroids which R uses to define hexagon layer

ConPoly_hex = gBuffer(ConPoly_hex, width = 0)
HexPts <- spsample(ConPoly_hex, type="hexagonal", cellsize = dist_between_centroids, offset = c(0, 0))
HexPols <- HexPoints2SpatialPolygons(HexPts)
plot(HexPols)

row.names(HexPols) <- as.character(1:length(HexPols))
Poly_ID <- 1:length(HexPols)

HexPols <- SpatialPolygonsDataFrame(HexPols, as.data.frame(Poly_ID))
ConPoly <- HexPols

rm(grid, ConPoly_ID, EEZ, hex_area, dist_between_centroids, Poly_ID, ConPoly_hex, HexPts, HexPols)

writeOGR(ConPoly, dsn = "./output", layer = "Hexagon_grid", 
         driver = "ESRI Shapefile", verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)

