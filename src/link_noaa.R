# Link data from NOAA (e;g. cloud cover and atmospheric pressure) to dataset
# By Pieterjan Verhelst and Damiano Oldoni 
# Pieterjan.Verhelst@UGent.be and damiano.oldoni@inbo.be


# Load packages
library(tidyverse)
library(assertthat)
library(glue)
library(lubridate)
library(worldmet)
library(sf)
library(purrr)
library(furrr)
source("./src/noaa_functions.R")

# set up multiprocessing
future::plan(multisession)

# define parameters/thresholds
spatial_threshold_in_kilometers <- 50
temporal_threshold_in_hours <- 2

# Check stations on map
# info <- getMeta(lat = 55.5, lon = 7.5)
info <- getMeta(lat = 51.2, lon = 3)

# 1. Define NOAA stations ####

# Stations to remove when downloading data from 2012:2013 :
#ula = "014281-99999"
#harald = "060224-99999"
#gormc = "060221-99999"
#halfdan = "060222-99999"
#zeebrugge = "064180-99999"
#landsend = "038060-99999"

station_codes <- list(
  andrew = "031405-99999",
  sleipner = "010886-99999",
  lerwick = "030050-99999",
  gullfax = "013755-99999",
  bruce = "031402-99999",
  heimdal = "010875-99999",
  harding = "031403-99999",
  #ula = "014281-99999",
  trollc = "010887-99999",
  trolla = "010877-99999",
  utsirafyr= "014030-99999",
  lista = "014270-99999",
  lindesnes = "014360-99999",
  marnock = "031407-99999",
  mungo = "031406-99999",
  ekofisk = "014000-99999",
  ekofiskoil = "014033-99999",
  a12 = "062050-99999",
  #harald = "060224-99999",
  tyrae = "060223-99999",
  #gormc = "060221-99999",
  #halfdan = "060222-99999",
  thyboroen = "060520-99999",
  hvide = "060580-99999",
  hornsb = "060170-99999",
  hornsa = "060160-99999",
  westerland = "100180-99999",
  nordholz = "101360-99999",
  f3 = "062390-99999",
  j6 = "062110-99999",
  d15 = "062010-99999",
  f16 = "062060-99999",
  l9 = "062070-99999",
  vlieland = "062420-99999",
  helipad = "062120-99999",
  k14 = "062040-99999",
  ruyter = "062030-99999",
  #zeebrugge = "064180-99999",
  oostende = "064070-99999",
  landwick = "036930-99999",
  koksijde = "064000-99999",
  dunkerque = "070100-99999",
  boulogne = "070020-99999",
  touqet = "070030-99999",
  lydd = "038873-99999",
  dieppe = "070400-99999",
  shoreham = "038760-99999",
  maupertus = "070240-99999",
  haumet = "070220-99999",
  hague = "070200-99999",
  alderney = "038915-99999",
  carteret = "070340-99999",
  portland = "038570-99999",
  guernsey = "038940-99999",
  jersey = "038950-99999",
  plymouth = "038270-99999",
  brehat = "071210-99999",
  armor = "071200-99999",
  ploumanach = "071170-99999",
  culdrose = "038090-99999",
  batz = "071160-99999",
  brignogan = "071070-99999",
  ouessant = "071000-99999",
  #landsend = "038060-99999",
  scilly = "038030-99999",
  mathieu = "071020-99999",
  raz = "071030-99999",
  penmarch = "072000-99999",
  melen = "072030-99999"
)

# Short version for testing
#station_codes <- list(
#  haumet = "070220-99999"
#)

# Path where NOAA data files are stored: directory will be created if not
# exists. If it exists, an informative warning is returned
noaa_path <- "./data/external/noaa"
dir.create(noaa_path, showWarnings = TRUE)

# 2. Download and combine datasets ####
noaa <- map2_dfr(station_codes,
                names(station_codes),
                get_data_noaa,
                hourly_data = TRUE, 
                years = 2012:2013,
                path = noaa_path)

# Reference code for one station
# andrew <- importNOAA(code = "031405-99999", hourly = TRUE, year = 2018:2019)
# andrew <- select(andrew, station, date, latitude, longitude, atmos_pres, cl)

# 3. Load dataset with all eels ####
data <- read.csv("./data/interim/data_circadian_tidal_5min.csv")
data$X <- NULL
data$ID <- factor(data$ID)
data$datetime  <- as_datetime(data$datetime)
data$Country <- factor(data$Country)

# Filter German data
data <- filter(data, Country == "Germany")

# Filter 1 animal for testing
#data <- filter(data, ID == "16031")

# Check NAs
sum(is.na(noaa$cl))  
sum(is.na(noaa$latitude))  
sum(is.na(noaa$longitude))  

sum(is.na(data$avg_lat))  
sum(is.na(data$avg_lon))  

# Remove NAs 
noaa <- noaa[!is.na(noaa$cl), ]
noaa <- noaa[!is.na(noaa$latitude), ]
noaa <- noaa[!is.na(noaa$longitude), ]

data <- data[!is.na(data$avg_lat), ]
data <- data[!is.na(data$avg_lon), ]

# check "metadata" from noaa
noaa %>%
  distinct(code, station, latitude, longitude)

# 4. Link environmental NOAA data to DST tracks

# "chop" data based on coordinates and create GIS layers from the tracks and
# NOAA stations
dst_tracks <- 
  data %>%
  mutate(latitude = avg_lat,
         longitude = avg_lon) %>%
  group_by(avg_lat,
           avg_lon) %>%
  chop() %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
  st_transform(crs = 3035)

dst_tracks

env_stations_sf <- 
  noaa %>%
  group_by(code, station, longitude, latitude) %>%
  nest() %>%
  st_as_sf(coords = c("longitude",
                      "latitude"),
           crs = 4326) %>%
  st_transform(crs = 3035)

env_stations_sf

# create distance matrix (df)
dist_df <- 
  dst_tracks %>% 
  st_distance(env_stations_sf) %>%
  as_tibble()

#' assign names based on env station codes (ensures uniqueness and traceability)
#' Eventually could we opt for the nicknames by:
#' names(dist_df) <- names(station_codes)
names(dist_df) <- as.character(env_stations_sf$code)

dist_df

remove(dst_tracks)

# Add row ID to tracking data (unique identifier, useful for linking env station
# data to trackign data)
data <- data %>%
  rownames_to_column(var = "row_id") %>%
  mutate(row_id = as.numeric(row_id))

# retrieve best fitting environmental data
env_data <- 
  future_map2_dfr(data$row_id,
           data$datetime,
           function(rowID, dt) {
             x_y_tracked <- list(x = data[data$row_id == rowID,]$avg_lon,
                                 y = data[data$row_id == rowID,]$avg_lat)
             # get stations in the neighborhood
             near_stations <- get_nearest_stations(
               rowID = rowID,
               dist_threshold = spatial_threshold_in_kilometers,
               distance_df = dist_df,
               tracking_data_lon = x_y_tracked$x,
               tracking_data_lat = x_y_tracked$y)
             
             # find the best fitting environmental data (geographically and temporally)
             env_data_to_add <- get_best_env_data(
               datetime_track = dt,
               ordered_noaa_stations = near_stations,
               timethreshold_hours = temporal_threshold_in_hours, 
               rowID = rowID,
               df_noaa_stations = noaa)
           }
)

# rename some returned columns
env_data <-
  env_data %>%
  rename(noaa_station_lat = latitude,
         noaa_station_lon = longitude,
         noaa_date = date)

# check number of rows
assertthat::assert_that(nrow(data) == nrow(env_data),
                        msg = glue("DST tracking data and environmental data must contain same number of rows"))

# Join environmental data to DST tracking data
data <-
  data %>%
  left_join(env_data, by = "row_id")
data
