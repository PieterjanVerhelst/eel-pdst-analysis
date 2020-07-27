# Link circadian phases to the dataset
# By Pieterjan Verhelst and Damiano Oldoni
# Pieterjan.Verhelst@UGent.be, damiano.oldoni@inbo.be



# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()


# Packages
library(tidyverse)
library(lubridate)
library(suncalc)



# 1. Load sensor dataset with all eels ####
data <- read_csv("./data/interim/batch_processed_eels.csv")

# Process columns
data$Date <- as.Date(data$datetime)
data$ID <- gsub( "A0", "", as.character(data$ID))
data$ID <- gsub( "A", "", as.character(data$ID))
data$ID <- factor(data$ID)


# 2. Load trajectory dataset with coordinates ####
tr_data <-  list.files(path = "./data/external/trajectory_data/",
                       pattern = "*.csv", 
                       full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# Select columns
tr_data <- select(tr_data, ID, Date, MPL.Avg.Lat, MPL.Avg.Lon, Med.Lat, Med.Lon)
tr_data <- rename(tr_data, 
                  avg_lat = MPL.Avg.Lat,
                  avg_lon = MPL.Avg.Lon,
                  med_lat = Med.Lat,
                  med_lon = Med.Lon)

# Process columns
tr_data$Date <- as.Date(tr_data$Date)
tr_data$ID <- factor(tr_data$ID)

# Remove double dates per eel (ID)
tr_data <- tr_data[!duplicated(tr_data[c('ID','Date')]),]

# Select relevant eels
tr_data <- filter(tr_data, ID == "9359" |
                    ID == "15714" |
                    ID == "15777" |
                    ID == "16031")
tr_data$ID <- factor(tr_data$ID) # rerun 'factor()' so the number of levels is set accurately


# 3. Merge datasets ####
data_circ <- left_join(x = data, y = tr_data, by=c("ID","Date"))
data_circ$avg_lat <- as.numeric(data_circ$avg_lat)  # important to convert lat and lon to numeric for getSunlightTimes()
data_circ$avg_lon <- as.numeric(data_circ$avg_lon)
#plot(data_circ$avg_lon, data_circ$avg_lat)



# 4. Link circadian phases to dataset ####
# Calculate sunrise and sunset for each date + position
m <- select(data_circ, Date, avg_lat, avg_lon)
m <- m %>% 
  rename(
    date = Date,
    lat= avg_lat,
    lon = avg_lon
  )

sun <- getSunlightTimes(data = m, tz = "UTC", keep = c("sunrise", "sunset"))
sun <- distinct(sun) 

sun <- sun %>% 
  rename(
    Date = date,
    avg_lat= lat,
    avg_lon = lon
  )

# Merge sunrise and sunset data to data_circ dataset
data_circ <- left_join(x = data_circ, y = sun, by=c("Date","avg_lat","avg_lon"))

# Get ordered series of sunsets and sunrises
series_sunsets <- unique(data_circ$sunset)
series_sunsets <- series_sunsets[order(series_sunsets)]

series_sunrises <- unique(data_circ$sunrise)
series_sunrises <- series_sunsets[order(series_sunrises)]

# Transform to df (tibble)
series_sunsets <- tibble(sunset = series_sunsets)
series_sunrises <- tibble(sunrise = series_sunrises)

# Add previous sunset
series_sunsets <-
  series_sunsets %>%
  mutate(previous_sunset = lag(sunset))

# Add next sunrise
series_sunrises <-
  series_sunrises %>%
  mutate(next_sunrise = lead(sunrise))

# Identify day and night periods by using series_sunsets
data_circ <-
  data_circ %>%
  left_join(series_sunsets,
            by = c("sunset"))

data_circ <-
  data_circ %>%
  left_join(series_sunrises,
            by = c("sunrise"))

data_circ <-
  data_circ %>%
  mutate(start_sunmoment = case_when(
    datetime < sunrise ~ previous_sunset,
    datetime > sunset ~ sunset,
    TRUE ~ sunrise),
    next_sunmoment = case_when(
      datetime > sunset ~ next_sunrise,
      datetime < sunrise ~ sunrise,
      TRUE ~ sunset))

# Add a progressive id to each night and day for each animal
data_circ <-
  data_circ %>%
  distinct(start_sunmoment) %>%
  arrange(start_sunmoment) %>%
  mutate(n_day_phase = row_number()) %>%
  right_join(data_circ,
             by = "start_sunmoment")

# Add night_day column instead of 
# data_circ$night_day <- ifelse(data_circ$datetime > data_circ$sunrise & data_circ$datetime < data_circ$sunset, 'day', 'night')
data_circ <-
  data_circ %>%
  mutate(nigth_day = if_else(datetime > sunset |
                               datetime < sunrise,
                             "night",
                             "day"))

# Set sunset of day before (= start_sunmoment) and  sunrise of day after for all values of the night
data_circ <-
  data_circ %>%
  mutate(sunset = if_else(nigth_day == "night",
                          start_sunmoment,
                          sunset),
         sunrise = if_else(nigth_day == "night",
                           next_sunmoment,
                           sunrise))


# Rearrange columns
data_circ <- select(data_circ, ID, datetime, numericdate, corrected_depth, temperature, Date, avg_lat, avg_lon, med_lat, med_lon, sunrise, sunset, previous_sunset, next_sunrise, start_sunmoment, next_sunmoment, nigth_day)
data_circ <- rename(data_circ, night_day = nigth_day)



# 5. write csv ####
write.csv(data_circ, "./data/interim/data_circadian.csv")




