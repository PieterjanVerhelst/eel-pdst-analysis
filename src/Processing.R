# Upload and process files
# Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be

library(tidyverse)


#rm(list = ls())

# Upload a single dataset to see structure of the data
eel1 <- read.csv("./data/raw/A15700_10-01-2019.csv", sep = ' ')


# path to folder that holds multiple .csv files
folder <- "./data/raw/"      

# create list of all .csv files in folder
file_list <- list.files(path = folder, pattern="*.csv", full.names=TRUE)




# Use lapply to upload all data in one list
data <- lapply(file_list, read.csv, sep = ' ', header = TRUE, stringsAsFactors = FALSE)



# read in each .csv file in file_list
for (i in 1:length(file_list)){
  
  assign(file_list[i], 
         
         read.csv(paste(folder, file_list[i], sep=''), sep = ',')
         
  )}




