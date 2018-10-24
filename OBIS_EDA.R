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
rm(OBIS_1, OBIS_2, OBIS_3, OBIS_4)
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

#Workflow for next steps:
#1. Chris will do spatial clip
#2. After this, the dataframe will have column called 'poly_id' or something - 
#3. Cole will,for each poly_id, 
#get the number of chordates, number of arthropods, and number of all else - have this in a 
#dataframe with column names 'polyid', 'chordate', 'arthropods', 'other' 
#4. Cole to save existing histograms to figures 
