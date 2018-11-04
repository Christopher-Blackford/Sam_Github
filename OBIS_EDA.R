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
obisnames<- c('yearcollected','genus', 'id', 'class', 'phylum', 'family', 'order', 'species', 'decimalLongitude', 'decimalLatitude', 'obisID', 'countryCode', 'catalogNumber')
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
summary(OBIS$yearcollected)

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

########################
########################


###############Subset dataframe into 5 year blocks
summary(OBIS$yearcollected)

#use the floor function to round down, along with the mutate function which is adding a new column

OBIS <- OBIS %>% 
  mutate(year5 = floor(yearcollected / 5) * 5)

############### New Heat Maps for Capelin, Cod, and Seabirds
names(OBIS)
## New data frames for Capelin, cod and seabirds
CodOBIS <- OBIS %>% 
       filter(species == 'Gadus morhua')
CapelinOBIS <- OBIS %>%
        filter(species == 'Mallotus villosus')
#see below about the two seabird dataframes
consbirdsOBIS <- OBIS %>% 
        filter(class == 'Aves', 
               !family %in% c('Cathartidae','Corvidae','Falconidae','Fringillidae', 
                 'Emberizidae','Hirundinidae','Icteridae','Parulidae','Troglodytidae',
                 'Alaudidae','Apodidae','Caprimulgidae','Cardinalidae','Cuculidae',
                 'Mimidae','Turdidae','Tyrannidae'))
libbirdsOBIS <- OBIS %>% 
  filter(class == 'Aves', 
         !family %in% c('Anatidae','Phalaropes','Phalaropodidae','Accipitridae','Cathartidae','Ardeidae','Corvidae','Falconidae',
                        'Fringillidae', 'Podicipedidae','Recurvirostridae', 'Scolopacidae',
                        'Threskiornithidae', 'Phaethontidae','Aramidae',
                        'Emberizidae','Hirundinidae','Icteridae','Parulidae','Troglodytidae',
                        'Alaudidae','Apodidae','Caprimulgidae','Cardinalidae','Cuculidae',
                        'Mimidae','Turdidae','Tyrannidae'))


# Based on the info below, I excluded a number of these families, I made two 
#dataframes, a conservative one with as many reasonable taxa as possible,
#and a liberal one with lots excluded, the ones I excluded or included
#are obvious in the code

# Some Notes on the Different Families:
    # Alcidae is the auks
    # Anatidae is the ducks and swans
    # Gaviidae is the loons
    # Hydrobatidae is the northern storm petrel
    # Laridae is the gulls
    # Phalacrocoracidae is the cormorants
    # Phalaropodidae is shorebirds 
    # Procellariidae includes petrels and the like 
    # Stercoraridae is the skuas
    # Sternidae is the terns
    # Sulidae is the gannetts and boobies
#   Phalaropes is shorebirds
#   Accipitridae is hawks and eagles and such, not quite seabirds?
#   Ardeidae is the herons
#   Cathartidae is vultures/condors
#   Charadriidae is plovers etc. (shorebirds)
#   Corvidae a family of songbirds (jays)
#   Falconidae is the falcons
#   Fringillidae is finches
#   Podicipedidae is the grebes
#   Recurvirostridae is wading shorebirds
#   Scolopacidae is sandpipers
#   Threskiornithidae is spoonbills (waders)
#   Emberizidae is a group of passerine songbirds
#   Hirundinidae is the swallows
#   Icteridae is passerine songbirds
#   Parulidae is warblers
#   Phaethontidae are the tropicbirds, a group of seabirds, but are native to the tropics
#   Troglodytidae are wrens
#   Alaudidae are larks
#   Apodidae are swifts
#   Aramidae - monotypic taxa thats a tropical shorebird
#   Caprimulgidae is the nightjars
#   Cardinalidae is cardinals
#   Cuculidae is the cuckoos
#   Fregatidae is a tropical and sub-tropical seabird 
#   Gruidae is the cranes
#   Mimidae is a group of songbirds
#   Turdidae is the thrushes
#   Tyrannidae is the largest group od songbirds

## add columns to OBIS dataframe for each bird grouping to allow for easy addition to the PolyDf
OBIS <- OBIS %>% 
  mutate(., LibBird = ifelse(family %in% c('Alcidae', 'Gaviidae','Hydrobatidae',
                                           'Laridae','Phalacrocoracidae','Procellariidae',
                                           'Stercoraridae','Sternidae','Sulidae'), 'Lib', 'No'))

OBIS <- OBIS %>% 
  mutate(., ConsBirds = ifelse(family %in% c('Alcidae', 'Anatidae','Gaviidae','Hydrobatidae',
                                             'Laridae','Phalacrocoracidae','Phalaropodidae','Procellariidae',
                                             'Stercoraridae','Sternidae','Sulidae','Phalaropes','Ardeidae','Charadriidae',
                                             'Podicipedidae','Recurvirostridae','Scolopacidae','Threskiornithidae',
                                             'Phaethontidae','Aramidae','Fregatidae'), 'Cons', 'No'))



## Plot for Seabirds
consbirdsOBISp1 <- consbirdsOBIS %>% 
  group_by(family) %>% 
  ggplot(aes(x = family, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
consbirdsOBISp1 <- consbirdsOBISp1 + theme(axis.text.x = element_text(angle = 90, size = 5))
consbirdsOBISp1

libbirdsOBISp1 <- libbirdsOBIS %>% 
  group_by(family) %>% 
  ggplot(aes(x = family, na.rm = TRUE)) +
  geom_bar(colour = 'black', fill = 'red2') +
  theme_classic()
libbirdsOBISp1 <- libbirdsOBISp1 + theme(axis.text.x = element_text(angle = 90, size = 5))
libbirdsOBISp1

########################
########################
#Add the capelin, both seabirds, and cod to the polydf dataframe so we have counts per polygon


Polydf <- OBIS %>% 
  group_by(., Poly_ID) %>% 
  summarize(., Chordates = sum(phylum == 'Chordata'),
            Arthropods = sum(phylum == 'Arthropoda'),
            AllElse = sum(phylum != 'Chordata' & phylum != 'Arthropoda'), 
            Cod = sum(species == 'Gadus morhua', na.rm = TRUE),
            Capelin = sum(species == 'Mallotus villosus', na.rm = TRUE),
            ConsBirds = sum(ConsBirds == 'Cons'),
            LibBird = sum(LibBird == 'Lib'))



