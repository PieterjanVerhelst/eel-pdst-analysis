# Upload and process files
# Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be

library(tidyverse)


rm(list = ls())


eel1 <- read.csv("./data/raw/A15700_10-01-2019.csv", sep = ' ')


# path to folder that holds multiple .csv files
folder <- "./data/raw/"      

# create list of all .csv files in folder
file_list <- list.files(path = folder, pattern="*.csv", full.names=TRUE)







data <- lapply(file_list, read.csv, sep = ',', header = TRUE, stringsAsFactors = FALSE)



# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  
  assign(file_list[i], 
         
         read.csv(paste(folder, file_list[i], sep=''), sep = ',')
         
  )}




filenames <- list.files('./data/raw/', pattern="*.csv", full.names=TRUE)
la <- lapply(filenames, read.csv)
