
###Loading packages
library(robis)
library(tidyverse)
library(rgbif)
library(data.table)

getwd()
memory.limit(size=25000) #Increase memory limit

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



#######RGBIF data download

occ_download_get("0005369-181003121212138") #https://doi.org/10.15468/dl.bxcxjr 

gbif_data <- data.table::fread("K:/Sam_project/Sam_Github/LargeData/GBIF/0005369-181003121212138.csv", na.strings = c("", NA)) 

excel_rows <- seq(from = 1, to = nrow(gbif_data), by = 1*10^6) #This rounds down so for example it only does to 3 million rows if the dataframe has 3 000 001 rows

for (i in 1:length(excel_rows)){
  if (i < length(excel_rows)){df <- gbif_data[c(excel_rows[i]:excel_rows[i+1]-1),]} #Splitting into rows from 1-1000000, 1000001-2000000 etc.
  else if (i == length(excel_rows)){df <- gbif_data[c(excel_rows[i]:nrow(gbif_data)),]} #Splitting the final document that probably doesn't fit evenly into a million rows
  
  write.csv(df, paste0("./LargeData/GBIF/GBIFData_part", i, "of", length(excel_rows), ".csv"), row.names = F)
}

