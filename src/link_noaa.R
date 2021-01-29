# Link data from NOAA (e;g. cloud cover and atmospheric pressure) to dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Load packages
library(tidyverse)
library(glue)
library(lubridate)
library(worldmet)
library(sf)
library(purrr)

# Check stations on map
info <- getMeta(lat = 55.5, lon = 7.5)

# 1. Download NOAA data ####
station_codes <- list(
  andrew = "031405-99999",
  sleipner = "010886-99999",
  lerwick = "030050-99999")
,
  gullfax = "013755-99999",
  bruce = "031402-99999",
  heimdal = "010875-99999",
  harding = "031403-99999",
  ula = "014281-99999",
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
  harald = "060224-99999",
  tyrae = "060223-99999",
  gormc = "060221-99999",
  halfdan = "060222-99999",
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
  zeebrugge = "064180-99999",
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
  landsend = "038060-99999",
  scilly = "038030-99999",
  mathieu = "071020-99999",
  raz = "071030-99999",
  penmarch = "072000-99999",
  melen = "072030-99999"
)

# wrap function around importNOAA() to get data we want
get_data_noaa <- function(station_code, 
                          station_nickname, 
                          hourly_data, 
                          years) {
  message(glue("Importing data of station '{ nickname }' ({ code })",
               nickname = station_nickname,
               code = station_code))
  data <- importNOAA(code = station_code, hourly = hourly_data, year = years)
  if (!"cl" %in% names(data)) {
    warning(glue("Station {station} has no column 'cl'. Created by duplicating values of 'cl_1'."),
            station = station_code)
    # haumet station (070220-99999) has no cl
    data$cl <- data$cl_1
  }
  data %>% 
    select(station, date, latitude, longitude, atmos_pres, cl_1, cl)
}

# 2. Combine datasets ####
noaa<- map2_dfr(station_codes,
                names(station_codes),
                get_data_noaa,
                hourly_data = TRUE, 
                years = 2018:2019)

# 3. Load dataset with all eels ####
data <- read.csv("./data/interim/data_circadian_tidal.csv")
data$X <- NULL
data$ID <- factor(data$ID)


# Filter 1 animal for testing
subset <- filter(data, ID == "16031")
summary(subset$direction)

# Check NAs
sum(is.na(noaa$atmos_pres))
sum(is.na(noaa$cl_1))  
sum(is.na(noaa$cl))  
sum(is.na(noaa$latitude))  
sum(is.na(noaa$longitude))  

# Remove unnecessary columns
noaa$atmos_pres <- NULL
noaa$cl_1 <- NULL

# Remove NAs 
noaa <- noaa[!is.na(noaa$cl), ]




# 4. Link environmental NOAA data to DST tracks

# create GIS layers from the tracks and NOAA stations
det_stations <- st_as_sf(subset,
                         coords = c("avg_lon",
                                    "avg_lat"),
                         crs = 4326)

env_stations <- st_as_sf(noaa,
                         coords = c("longitude",
                                    "latitude"),
                         crs = 4326)




