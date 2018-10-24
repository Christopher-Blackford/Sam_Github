#install.packages("devtools")
library(devtools)
#install_github("iobis/robis")
library(robis)
library(tidyverse)
install.packages('rgbif')
library(rgbif)
install.packages("ff")
library(ff)
setwd("C:/Users/coleb/Dropbox/2018MJFWorkStudy/ChrisConnectivityWork")

#Change the amount of memory being used
memory.limit(size = 25000) 
#######OBIS data download
obisdata <- occurrence(geometry = 
"POLYGON ((-69.14795 44.02442, -67.71973 41.45920, 
-61.52344 40.01079, -53.17383 41.57436, -47.37305 46.31658, 
-46.31836 50.62507, -48.60352 54.74999, -53.65723 58.63122, 
-63.32520 59.04055, -73.87207 46.76997, -69.14795 44.02442))")

names(obisdata)


obisdatavars <- c('id',
         'decimalLongitude',
         'decimalLatitude',
         'catalogNumber',
         'datasetName',
         'obisID',
         'scientificNameID',
         'genus',
         'species',
         'speciesID',
         'sex',
         'datasetID')
obisdata <- obisdata[obisdatavars]

obisdata <- read.csv.ffdf(file = "file:///C:/Users/coleb/Dropbox/2018MJFWorkStudy/ChrisConnectivityWork/OBIS/OBISStuff.csv")



#######RGBIF data download

occ_download_import(x = NULL, key = NULL, 
                    path = "C:\Users\coleb\Dropbox\2018MJFWorkStudy\ChrisConnectivityWork\OBIS\0003507-181003121212138.zip", fill = FALSE)




############################################### Combining files and creating plots ################################

setwd("C:/Users/coleb/Documents/GitHub/Sam_Github/LargeData/OBIS")

#Obis Data
OBIS_1 <- read.csv("OBISData_part1of4.csv")
OBIS_2 <- read.csv("OBISData_part2of4.csv")
OBIS_3 <- read.csv("OBISData_part3of4.csv")
OBIS_4 <- read.csv("OBISData_part4of4.csv")

OBIS <- rbind(OBIS_1, OBIS_2, OBIS_3, OBIS_4)
rm(OBIS_1, OBIS_2, OBIS_3, OBIS_4, p, OBIS_noinsects)
names(OBIS)
obisnames<- c('id', 'class', 'phylum', 'family', 'order', 'species', 'decimalLongitude', 'decimalLatitude', 'obisID', 'countryCode', 'catalogNumber')
OBIS <- OBIS[obisnames]
## Subset the data according to the different dataframes we want
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
hist(OBISarth$class)

OBISarthclassplot <- OBISarth %>%
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar() +
  theme_classic()

OBISarthorderplot <- OBISarth %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar() +
  theme_classic()

OBISchordclassplot <- OBISchord %>% 
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar() +
  theme_classic()

OBISchordorderplot <- OBISchord %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar() +
  theme_classic()

OBISallelseclassplot <- OBISallelse %>% 
  group_by(class) %>% 
  ggplot(aes(x = class, na.rm = TRUE)) +
  geom_bar() +
  theme_classic()

OBISallelseorderplot <- OBISallelse %>% 
  group_by(order) %>% 
  ggplot(aes(x = order, na.rm = TRUE)) +
  geom_bar() +
  theme_classic()

OBISarthclassplot
OBISarthorderplot
OBISchordclassplot
OBISchordorderplot
OBISallelseclassplot
OBISallelseorderplot
