
###Loading packages
library(devtools)
#install_github("iobis/robis")
library(robis)
library(tidyverse)
library(rgbif)
#library(ff)

getwd()

#######OBIS data download
obisdata <- occurrence(geometry = 
"POLYGON ((-69.14795 44.02442, -67.71973 41.45920, 
-61.52344 40.01079, -53.17383 41.57436, -47.37305 46.31658, 
-46.31836 50.62507, -48.60352 54.74999, -53.65723 58.63122, 
-63.32520 59.04055, -73.87207 46.76997, -69.14795 44.02442))")

df1 <- obisdata[c(1:1000000),]
write.csv(df1, "./output/LargeData/df1.csv")

df2 <- obisdata[c(1000001:2000000),]
write.csv(df2, "./output/LargeData/df2.csv")

df3 <- obisdata[c(2000001:3901265),]
write.csv(df3, "./output/LargeData/df3.csv")





smallerobisdatavars <- c('id',
                  'decimalLongitude',
                  'decimalLatitude',
                  'catalogNumber',
                  'datasetName',
                  'obisID',
                  'scientificNameID',
                  'genus',
                  'family',
                  'order',
                  'species',
                  'speciesID',
                  'sex',
                  'datasetID')
smallerobisdata <- smallerobisdata[smallerobisdatavars]

frstprtsmallerobda <- smallerobisdata[c(1:15000),]
scndprtsmallerobda <- smallerobisdata[c(15001:30000),]
thrdprtsmallerobda <- smallerobisdata[c(30001:44510),]

write.csv(frstprtsmallerobda, file = "C:/Users/coleb/Documents/GitHub/Sam_Github/frstprtsmallerobda.csv")
write.csv(scndprtsmallerobda, file = "C:/Users/coleb/Documents/GitHub/Sam_Github/scndprtsmallerobda.csv")
write.csv(thrdprtsmallerobda, file = "C:/Users/coleb/Documents/GitHub/Sam_Github/thrdprtsmallerobda.csv")



#might want to break it down into total occurences of different families, and then maybe number 
#of families in each hexagon cells. 


names(obisdata)


obisdatavars <- c('id',
         'decimalLongitude',
         'decimalLatitude',
         'catalogNumber',
         'datasetName',
         'obisID',
         'scientificNameID',
         'genus',
         'family',
         'order',
         'species',
         'speciesID',
         'sex',
         'datasetID')
obisdata <- obisdata[obisdatavars]

obisdata <- read.csv.ffdf(file = "file:///C:/Users/coleb/Dropbox/2018MJFWorkStudy/ChrisConnectivityWork/OBIS/OBISStuff.csv")



#######RGBIF data download

occ_download(geometry = "POLYGON ((-60.6775 47.3463, -59.6997 46.3697, -57.7441 46.9353, 
-59.8975 47.7984, -60.6775 47.3463))")

occ_download_import(x = NULL, key = NULL, 
                    path = "C:\Users\coleb\Dropbox\2018MJFWorkStudy\ChrisConnectivityWork\OBIS\0003507-181003121212138.zip", fill = FALSE)




