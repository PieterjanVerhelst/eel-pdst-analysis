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


# 3. write csv ####
write.csv(data, "./data/interim/data_circadian_tidal_moon_5min.csv")

