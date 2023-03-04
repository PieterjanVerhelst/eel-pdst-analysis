# Classify current phases
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) 
library(lubridate)


# 1. Read data ####
data <- read.csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data <- data %>%
  rename(direction_x = U,
         direction_y = V)

# Arrange data set according to ID and datetime
data <- data %>% 
  arrange(ID, datetime)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]

# Remove rows with NA in direction column
data <- data %>% drop_na(direction)

# Classify currents
# Current in x-direction
#data$current_phase_x <- NA
#for (i in 1:dim(data)[1]){
#  if (data$direction[i] >= 0){
#    data$current_phase_x[i] = "eastward"
#  } else if (data$direction[i] < 0){
#    data$current_phase_x[i] = "westward"
#  } else{
#    data$current_phase_x[i] = "NA"
#  }}

data$current_phase_x <- NA
for (i in 1:dim(data)[1]){
  if (data$direction_x[i] >= 0){
    data$current_phase_x[i] = "eastward"
  } else if (data$direction_x[i] < 0){
    data$current_phase_x[i] = "westward"
  } else{
    data$current_phase_x[i] = "NA"
  }}


# Current in y-direction
#data$direction_abs <- abs(data$direction)
#data$current_phase_y <- NA
#for (i in 1:dim(data)[1]){
#  if (data$direction_abs[i] <= 90){
#    data$current_phase_y[i] = "northward"
#  } else if (data$direction_abs[i] > 90){
#    data$current_phase_y[i] = "southward"
#  } else{
#    data$current_phase_y[i] = "NA"
#  }}

data$current_phase_y <- NA
for (i in 1:dim(data)[1]){
  if (data$direction_y[i] >= 0){
    data$current_phase_y[i] = "northward"
  } else if (data$direction_y[i] < 0){
    data$current_phase_y[i] = "southward"
  } else{
    data$current_phase_y[i] = "NA"
  }}



# Calculate the p parallel and t transverse with 25 degrees of in the direction of the English Channel
data$p_parallel <- (data$direction_x * cos(deg2rad(25))) + (data$direction_y * sin(deg2rad(25))) 
data$t_transverse <- (data$direction_x * sin(deg2rad(25))) + (data$direction_y * cos(deg2rad(25))) 

# Classify current in p_parallel

data$current_phase_p <- NA
for (i in 1:dim(data)[1]){
  if (data$direction_x[i] >= 0){
    data$current_phase_p[i] = "non-favourable"
  } else if (data$direction_x[i] < 0){
    data$current_phase_p[i] = "favourable"
  } else{
    data$current_phase_p[i] = "NA"
  }}



table(data$current_phase_x)
table(data$current_phase_y)


# write csv
write.csv(data, "./data/interim/data_current_phases.csv")


