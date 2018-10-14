
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

#Split larger data into smaller csvs, each with 1 000 000 rows (excel limit is aroun 1 046 000)
excel_rows <- seq(from = 1, to = nrow(obisdata), by = 1*10^6) #This rounds down so for example it only does to 3 million rows if the dataframe has 3 000 001 rows

for (i in 1:length(excel_rows)){
  if (i < length(excel_rows)){df <- obisdata[c(excel_rows[i]:excel_rows[i+1]-1),]} #Splitting into rows from 1-1000000, 1000001-2000000 etc.
  else if (i == length(excel_rows)){df <- obisdata[c(excel_rows[i]:nrow(obisdata)),]} #Splitting the final document that probably doesn't fit evenly into a million rows
  
  write.csv(df, paste0("./LargeData/OBIS/OBISData_part", i, "of", length(excel_rows), ".csv"), row.names = F)
}


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

#######RGBIF data download

gbif_download <- occ_download(user = "runcrispy", pwd = "14socialbutterflies", email = "runcrispy@gmail.com", 
                              'geometry = POLYGON((-71.10352 47.15984,-65.30273 42.29356,-50.18555 42.29356,-48.07617 51.06902,-55.45898 56.94497,-71.10352 47.15984))')


occ_download_meta(gbif_download)

#Tutorial
#occ_download(user = "runcrispy", pwd = "14socialbutterflies", email = "runcrispy@gmail.com", 'taxonKey = 7264332', 'hasCoordinate = TRUE')


#Searching (don't need)
gbif_occurence <- occ_search(geometry = 'POLYGON((-71.10352 47.15984,-65.30273 42.29356,-50.18555 42.29356,-48.07617 51.06902,-55.45898 56.94497,-71.10352 47.15984))')

?occ_data


write.csv(gbif_occurence, "./LargeData/GBIF/temp/csv")



