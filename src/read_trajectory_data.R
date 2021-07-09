# Batch read and process trajectory data obtained via matlab toolbox which is stored in ./data/external/trajectory_data
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)


# Read in trajectory data ####
tr_data <-  list.files(path = "./data/external/trajectory_data/individual_files/",
             pattern = "*.csv", 
             full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 



# Select columns
tr_data <- select(tr_data, ID, Date, MPL.Hori_Dist_km, MPL.Avg.Lat, MPL.Avg.Lon)
tr_data <- rename(tr_data, Distance = MPL.Hori_Dist_km,
                  Lat = MPL.Avg.Lat,
                  Lon = MPL.Avg.Lon)


# Remove NA
sum(is.na(tr_data$Distance))
tr_data <- na.omit(tr_data) 
