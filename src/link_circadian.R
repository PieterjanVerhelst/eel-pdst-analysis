# Link circadian phases to the dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()


# Packages
library(dplyr)
library(lubridate)
library(suncalc)



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
data_circ$circadian <- ifelse(data_circ$datetime2 > data_circ$sunrise & data_circ$datetime2 < data_circ$sunset, 'day', 'night')




# 5. Create plot with day night ####
# Create subsets of several days
subset <- filter(data_circ,
                 ID == "16031",
                 datetime2 >= "2019-02-04 00:00:00", datetime2 <= "2019-02-07 00:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime2[1], "day"), to= subset$datetime2[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime2[1], "day"))

# Create plot
fig_circadian <- ggplot(subset, aes(x = datetime2,
                                       y = temperature)) +
  geom_rect(aes(xmin=sunrise, xmax=sunset, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3) +
  geom_line(binaxis='x', size=1.0, binwidth = 1) +
  geom_line(data = subset, aes(x = datetime2, y = corrected_depth/2), size = 1.0, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Pressure (m)")) +
  theme_minimal() +
  ylab("Temperature (°C)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 hour") +
  #geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) 
fig_circadian


