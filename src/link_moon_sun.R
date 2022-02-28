# Link illuminated moon fraction, sun position and sun azimuth to the dataset
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
data <- read_csv("./data/interim/data_circadian_tidal_5min.csv",
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)

# 2. Link illuminated moon fraction to the dataset
moon_fraction <- data %>%
  select(ID, datetime) %>%
  mutate(getMoonIllumination(datetime, keep = "fraction"))

moon_fraction$date <- NULL
moon_fraction <- rename(moon_fraction, moon_fraction = fraction)


class(moon_fraction$datetime)
class(moon_fraction$moon_fraction)

data <- left_join(data, moon_fraction, by=c("ID", "datetime"))


# 3. Link sun position and azimuth to the dataset
# Prepare dataset with relevant columns and adjust column names
sun_position <- data %>%
  select(ID, datetime, geoloc_avg_lat, geoloc_avg_lon) %>%
  rename(date = datetime,      # column names need to be matching the names of the function (getSunlightPosition)
         lat = geoloc_avg_lat,
         lon = geoloc_avg_lon) 

# Feed this to calculation so the tag IDs are obtained
data <- data %>%
  #select(ID, datetime, geoloc_avg_lat, geoloc_avg_lon) %>%
  #rename(date = datetime,      # column names need to be matching the names of the function (getSunlightPosition)
  #       lat = geoloc_avg_lat,
  #       lon = geoloc_avg_lon) %>%
  mutate(getSunlightPosition(data = sun_position, keep = c("altitude", "azimuth")))

data$date <- NULL
data$lat <- NULL
data$lon <- NULL

data <- rename(data,
               sun_altitude = altitude,
               sun_azimuth = azimuth)


# 4. write csv ####
write.csv(data, "./data/interim/data_circadian_tidal_moon_sun_5min.csv")

