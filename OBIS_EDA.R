
###Loading packages
library(tidyverse)
library(rgdal)
library(rgeos)
library(data.table)

##Loading custom functions
source("./sub_code/functions/my_point_in_poly.R")

#Change the amount of memory being used
memory.limit(size = 25000) 


########################
########################
#1. Loading in OBIS files and clipping to study extent (Chris)

filenames <- list.files(path="LargeData/OBIS/", pattern= "OBISData_", full.names=TRUE, recursive=T)

# load all files into a list
#datalist <- lapply(filenames, read.csv)

OBIS <- NULL
for (i in 1:length(filenames)){temp <- read.csv(filenames[i])
  OBIS <- rbind(OBIS,temp)}
rm(temp)

names(OBIS)
obisnames<- c('id', 'class', 'phylum', 'family', 'order', 'species', 'decimalLongitude', 'decimalLatitude', 'obisID', 'countryCode', 'catalogNumber')
OBIS <- OBIS[obisnames]

###Spatial subset based on being within study extent
Study_Area_hex <- readOGR("./output/shapefiles", "Study_Area_hex")

xy <- subset(OBIS, select = c(decimalLongitude, decimalLatitude))

OBIS_observations <- SpatialPointsDataFrame(coords = xy, data = OBIS, proj4string = CRS("+proj=longlat +datum=WGS84"))
OBIS_observations <- spTransform(OBIS_observations, Study_Area_hex@proj4string)

#writeOGR(OBIS_observations, dsn = "./output", layer = "temp", driver = "ESRI Shapefile", verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)

OBIS_observations <- my.point.in.poly(OBIS_observations, Study_Area_hex)

OBIS <- OBIS_observations@data
rm(OBIS_observations, xy, i, filenames, obisnames)


########################
########################
#2. Subset the data according to the different dataframes we want. Make barplots of results

# Find all inverts except insects and arachnids, make a few data frames:
# 1. just arthropods
# 2. just chordates
# 3. everything that is not a chordate or an arthropod
# and then for each dataframe, do the histogram of the classes and orders!

#Step: filter arachnids and insects
OBIS <- OBIS %>% 
  filter(class != "Insecta", class != 'Arachnida') 


#Step: make a few dataframes
OBISarth <- OBIS %>% 
  filter(phylum == 'Arthropoda')
OBISchord <- OBIS %>% 
  filter(phylum == 'Chordata')
OBISallelse <- OBIS %>% 
  filter(phylum != 'Chordata', phylum != 'Eurarthropoda')
OBIS$phylum[1:40]
#Step: make plots

OBISarthclassplot <- OBISarth %>%
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBISarthclassplot <- OBISarthclassplot + theme(axis.text.x = element_text(angle = 90))


OBISarthorderplot <- OBISarth %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBISarthclassplot <- OBISarthclassplot + theme(axis.text.x = element_text(angle = 90))

OBISchordclassplot <- OBISchord %>% 
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBISchordclassplot <- OBISchordclassplot + theme(axis.text.x = element_text(angle = 90))

OBISchordorderplot <- OBISchord %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBISchordorderplot <- OBISchordorderplot + theme(axis.text.x = element_text(angle = 90))

OBISallelseclassplot <- OBISallelse %>% 
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBISallelseclassplot <- OBISallelseclassplot + theme(axis.text.x = element_text(angle = 90))

OBISallelseorderplot <- OBISallelse %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBISallelseorderplot <- OBISallelseorderplot + theme(axis.text.x = element_text(angle = 90, size = 3))


OBISarthclassplot
OBISarthorderplot
OBISchordclassplot
OBISchordorderplot
OBISallelseclassplot
OBISallelseorderplot

ggsave(OBISarthclassplot,
       file = './output/figures/OBISarthclassplot.png', dpi = 300)
ggsave(OBISarthorderplot,
       file = './output/figures/OBISarthorderplot.png', dpi = 300)
ggsave(OBISchordclassplot,
       file = './output/figures/OBISchordclassplot.png', dpi = 300)
ggsave(OBISchordorderplot,
       file = './output/figures/OBISchordorderplot.png', dpi = 300)
ggsave(OBISallelseclassplot,
       file = './output/figures/OBISallelseclassplot.png', dpi = 300)
ggsave(OBISallelsecorderplot,
       file = './output/figures/OBISallelseorderplot.png', dpi = 300)


########################
########################
#3. Get dataframe of # of chordates, number of arthropods, and number of all else for each Poly_ID (Cole)


########################
########################
#4. Spatial heatmap of # of chordates, number of arthropods, and number of all else (Chris)




#Workflow for next steps:
#1. Chris will do spatial clip
#2. After this, the dataframe will have column called 'poly_id' or something - 
#3. Cole will,for each poly_id, 
#get the number of chordates, number of arthropods, and number of all else - have this in a 
#dataframe with column names 'polyid', 'chordate', 'arthropods', 'other' 
#4. Cole to save existing histograms to figures 
