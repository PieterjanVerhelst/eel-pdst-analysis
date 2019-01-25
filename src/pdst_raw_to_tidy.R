#
# pdst sensor read functionalities
# 
# Van Hoey S. 2019
# Lifewatch INBO
# 

library(tidyverse)

# load the reading functions
source("./src/pdst_functions.R")

# get an overview of the files
main_data_dir <- "./data/raw"
data_files <- list.files(main_data_dir, full.names = TRUE)

# create interim dictionary if not already there
interim_data_dir <- file.path("data", "interim")
if (!dir.exists(interim_data_dir)) {
  dir.create(interim_data_dir)
}

# read data from the raw data files and convert to tidy -----

for (filename in data_files) {
  
  # use custom read functionality
  file_data <- pdst_read_file(filename)
  
  # write the daylog and sensor as tidy data files
  # (only write daylog if present in raw data file)
  if (is.data.frame(file_data$daylog)) {
    write_csv(file_data$daylog, 
              file.path(interim_data_dir, 
                        paste0("daylog_", basename(filename))))    
  }
  write_csv(file_data$sensor, 
            file.path(interim_data_dir, 
                      paste0("sensor_", basename(filename))))
}



