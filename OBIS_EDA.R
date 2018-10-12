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

