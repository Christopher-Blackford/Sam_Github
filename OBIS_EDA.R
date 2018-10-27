#setwd('C:/Users/coleb/Documents/GitHub/Sam_Github')

###Loading packages
library(tidyverse)
library(rgdal)
library(rgeos)
library(data.table)

###Loading custom functions
source("./sub_code/functions/my_point_in_poly.R")

###Allocate extra memory to R
memory.limit(size = 25000) 


########################
########################
# Loading in OBIS files and clipping to study extent (Chris)

filenames <- list.files(path="LargeData/OBIS/", pattern= "OBISData_", full.names=TRUE, recursive=T)

#datalist <- lapply(filenames, read.csv) # load all files into a list - slower than my loop?

OBIS <- NULL
for (i in 1:length(filenames)){temp <- read.csv(filenames[i]); OBIS <- rbind(OBIS,temp)}
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


#########################
########################
# Subset the data according to the different dataframes we want. Make barplots of results

# Find all inverts except insects and arachnids, make a few data frames:
# 1. just arthropods
# 2. just chordates
# 3. everything that is not a chordate or an arthropod
# and then for each dataframe, do the histogram of the classes and orders!

#Step: filter arachnids and insects
OBIS <- OBIS %>% 
  filter(class != "Insecta", class != 'Arachnida') 


#Step: make a few dataframes
OBIS_Arth <- OBIS %>% 
  filter(phylum == 'Arthropoda')
OBIS_Chord <- OBIS %>% 
  filter(phylum == 'Chordata')
OBIS_AllElse <- OBIS %>% 
  filter(phylum != 'Chordata', phylum != 'Arthropoda')


#Step: make plots
OBIS_ArthClass <- OBIS_Arth %>%
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBIS_ArthClass <- OBIS_ArthClass + theme(axis.text.x = element_text(angle = 90)); OBIS_ArthClass
ggsave(OBIS_ArthClass,
       file = './output/figures/OBIS_ArthClass.png', width = 16, height = 9, units = "cm", dpi = 300)

OBIS_ArthOrder <- OBIS_Arth %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBIS_ArthOrder <- OBIS_ArthOrder + theme(axis.text.x = element_text(angle = 90)); OBIS_ArthOrder
ggsave(OBIS_ArthOrder,
       file = './output/figures/OBIS_ArthOrder.png', width = 32, height = 18, units = "cm", dpi = 320)

OBIS_ChordClass <- OBIS_Chord %>% 
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBIS_ChordClass <- OBIS_ChordClass + theme(axis.text.x = element_text(angle = 90)); OBIS_ChordClass
ggsave(OBIS_ChordClass,
       file = './output/figures/OBIS_ChordClass.png', width = 16, height = 9, units = "cm", dpi = 300)

OBIS_ChordOrder <- OBIS_Chord %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBIS_ChordOrder <- OBIS_ChordOrder + theme(axis.text.x = element_text(angle = 90)); OBIS_ChordOrder
ggsave(OBIS_ChordOrder,
       file = './output/figures/OBIS_ChordOrder.png', width = 32, height = 18, units = "cm", dpi = 320)

OBIS_AllElseClass <- OBIS_AllElse %>% 
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBIS_AllElseClass <- OBIS_AllElseClass + theme(axis.text.x = element_text(angle = 90, size = 2)); 
ggsave(OBIS_AllElseClass,
       file = './output/figures/OBIS_AllElseClass.png', width = 16, height = 9, units = "cm", dpi = 320)

OBIS_AllElseOrder <- OBIS_AllElse %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
OBIS_AllElseOrder <- OBIS_AllElseOrder + theme(axis.text.x = element_text(angle = 90, size = 3))
ggsave(OBIS_AllElseOrder,
       file = './output/figures/OBIS_AllElseOrder.png', width = 32, height = 18, units = "cm", dpi = 320)


########################
########################
#Get dataframe of # of chordates, number of arthropods, and number of all else for each Poly_ID (Cole)

OBIS$Poly_ID <- as.factor(OBIS$Poly_ID)

Polydf <- OBIS %>% 
          group_by(., Poly_ID) %>% 
          summarize(., Chordates = sum(phylum == 'Chordata'),
                 Arthropods = sum(phylum == 'Arthropoda'),
                 AllElse = sum(phylum != 'Chordata' & phylum != 'Arthropoda'))

#write.csv(Polydf, "./output/Polydf.csv", row.names = F)

########################
########################
#4. Spatial heatmap of # of chordates, number of arthropods, and number of all else (Chris)
#Polydf <- read.csv("./output/Polydf.csv")

OBIS_Poly <- sp::merge(Study_Area_hex, Polydf)

writeOGR(OBIS_Poly, dsn = "./output/shapefiles/OBIS", layer = "OBIS_Poly", driver = "ESRI Shapefile", 
         verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)


