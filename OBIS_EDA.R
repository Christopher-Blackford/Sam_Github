#install_github("iobis/robis")
library(tidyverse)


#Change the amount of memory being used
memory.limit(size = 25000) 

setwd("./LargeData/OBIS")

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


#Workflow for next steps:
#1. Chris will do spatial clip
#2. new dataframe will have column called 'poly_id' or something - for each poly_id, 
#get the number of chordates, number of arthropods, and number of all else - have this in a 
#dataframe with column names 'polyid', 'chordate', 'arthropods', 'other' 
