# Batch read and process trajectory data obtained via matlab toolbox which is stored in ./data/external/trajectory_data
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)


# Read in trajectory data ####

# 1. For separate files (note these are not up to date!)
#tr_data <-  list.files(path = "./data/external/trajectory_data/individual_files/",
#             pattern = "*.csv", 
#             full.names = T) %>% 
#  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# 2. For a single file containing all eel tracks
tr_data <- read_csv("./data/external/trajectory_data/eel_trajectories.csv")


# Select columns
tr_data <- select(tr_data, ID, Date, MPL.Hori_Dist_km, MPL.Avg.Lat, MPL.Avg.Lon)
tr_data <- rename(tr_data, Distance = MPL.Hori_Dist_km,
                  Lat = MPL.Avg.Lat,
                  Lon = MPL.Avg.Lon)


# Set NA to zero
sum(is.na(tr_data$Distance))
tr_data$Distance[is.na(tr_data$Distance)] <- 0

