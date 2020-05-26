# Link circadian phases to the dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()

# Packages
library(suncalc)
library(lubridate)


# 1. Load sensor dataset with all eels ####
data <- read_csv("./data/interim/all_sensor_eels_processed.csv")

# Process columns
data$Date <- as.Date(data$datetime2)
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



# 3. Merge datasets ####
data_circ <- left_join(x = data, y = tr_data, by=c("ID","Date"))
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
data_circ$dayNight <- ifelse(data_circ$datetime2 > data_circ$sunrise & data_circ$datetime2 < data_circ$sunset, 'day', 'night')




