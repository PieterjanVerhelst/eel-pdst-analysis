# Link illuminated moon fraction, sun position and sun azimuth to the DVM dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()


# Packages
library(tidyverse)
library(lubridate)
library(suncalc)



# 1. Link illuminated moon fraction to the dataset ####
moon_fraction <- data_dvm %>%
  select(ID, datetime) %>%
  mutate(getMoonIllumination(datetime, keep = "fraction"))

moon_fraction$date <- NULL
moon_fraction <- rename(moon_fraction, moon_fraction = fraction)


class(moon_fraction$datetime)
class(moon_fraction$moon_fraction)

data_dvm <- left_join(data_dvm, moon_fraction, by=c("ID", "datetime"))


# 2. Link sun position and azimuth to the dataset  ####
# Prepare dataset with relevant columns and adjust column names
sun_position <- data_dvm %>%
  select(ID, datetime, geoloc_avg_lat, geoloc_avg_lon) %>%
  rename(date = datetime,      # column names need to be matching the names of the function (getSunlightPosition)
         lat = geoloc_avg_lat,
         lon = geoloc_avg_lon) 

# Feed this to calculation so the tag IDs are obtained
data_dvm <- data_dvm %>%
  #select(ID, datetime, geoloc_avg_lat, geoloc_avg_lon) %>%
  #rename(date = datetime,      # column names need to be matching the names of the function (getSunlightPosition)
  #       lat = geoloc_avg_lat,
  #       lon = geoloc_avg_lon) %>%
  mutate(getSunlightPosition(data = sun_position, keep = c("altitude", "azimuth")))

data_dvm$date <- NULL
data_dvm$lat <- NULL
data_dvm$lon <- NULL

data_dvm <- rename(data_dvm,
               sun_altitude = altitude,
               sun_azimuth = azimuth)

# Remove redundant columns
data_dvm$previous_sunset <- NULL
data_dvm$next_sunmoment <- NULL
data_dvm$next_sunrise <- NULL
data_dvm$start_sunmoment <- NULL


# 3. write csv ####
write.csv(data_dvm, "./data/interim/data_dvm_circadian_moon_sun.csv")

