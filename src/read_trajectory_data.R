# Batch read and process trajectory data obtained via matlab toolbox which is stored in ./data/external/trajectory_data
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

library(tidyverse)


# 1. Read in trajectory data ####
tr_data <- read_csv("./data/external/trajectory_data/EEL16031_Summary.csv")
