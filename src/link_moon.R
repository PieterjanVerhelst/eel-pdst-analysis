# Link illuminated moon fraction to the dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()


# Packages
library(tidyverse)
library(lubridate)
library(suncalc)


# 1. Import data ####
data <- read_csv("./data/interim/data_circadian_tidal.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)


# 2. Link illuminated moon fraction to the dataset
moon_fraction <- getMoonIllumination(data$datetime, keep = "fraction")
moon_fraction <- moon_fraction %>% 
  rename(
    datetime = date,
    moon_fraction = fraction
  )

class(moon_fraction$datetime)
class(moon_fraction$moon_fraction)

data <- merge(data, moon_fraction, by="datetime")


# 3. write csv ####
write.csv(data, "./data/interim/data_circadian_tidal_moon.csv")

