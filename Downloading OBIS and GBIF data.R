
###Loading packages
#library(devtools)
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

backup <- obisdata

#Split larger data into smaller csvs, each with 1 000 000 rows (excel limit is aroun 1 046 000)
excel_rows <- seq(from = 1, to = nrow(obisdata), by = 1*10^6) #This rounds down so for example it only does to 3 million rows if the dataframe has 3 000 001 rows

for (i in 1:length(excel_rows)){
  if (i < length(excel_rows)){df <- obisdata[c(excel_rows[i]:excel_rows[i+1]-1),]} #Splitting into rows from 1-1000000, 1000001-2000000 etc.
  else if (i == length(excel_rows)){df <- obisdata[c(excel_rows[i]:nrow(obisdata)),]} #Splitting the final document that probably doesn't fit evenly into a million rows
  
  write.csv(df, paste0("./LargeData/OBIS/OBISData_part", i, "of", length(excel_rows), ".csv"), row.names = F)
}


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

gbif_occurence <- occ_search(geometry = "POLYGON ((-60.6775 47.3463, -59.6997 46.3697, -57.7441 46.9353, 
                             -59.8975 47.7984, -60.6775 47.3463))")

gbif_download <- occ_download(gbif_occurence, user = "runcrispy", pwd = "14socialbutterflies", email = "runcrispy@gmail.com")


gbif_download <- occ_download(gbif_occurence, user = "runcrispy", pwd = "14socialbutterflies", email = "runcrispy@gmail.com")


occ_download_import(x = NULL, key = NULL, 
                    path = "C:\Users\coleb\Dropbox\2018MJFWorkStudy\ChrisConnectivityWork\OBIS\0003507-181003121212138.zip", fill = FALSE)

write.csv(gbif_occurence, "./LargeData/GBIF/temp/csv")



