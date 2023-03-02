# Create periodogram to identify main periods in the depth (time series) data
# Pieterjan Verhelst
# pieterjan.verhelst@inbo.be

library(tidyverse)
library(TSA)
library(zoo)  # To convert dataframe into time series
library(timeSeries)
library(imputeTS)  # For function 'na_remove()'


# 1. Read data ####
data <- read.csv("./data/interim/data_current_phases.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data$current_phase_x <- factor(data$current_phase_x)
data$current_phase_y <- factor(data$current_phase_y)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]

# Nordic eels
data <- filter(data, ID == "15805" |
                 ID == "15981" |
                 ID == "17492_2" |
                 ID == "17499" |
                 ID == "17525_2")

# Channel eels
data <- filter(data, ID != "15805" ,
               ID != "15981" ,
               ID != "17492_2" ,
               ID != "17499" ,
               ID != "17525_2")

# Arrange data set according to tag ID and datetime, so min and max are calculated accordingly
data <-
  data %>%
  arrange(ID, datetime)


# Calculate depth relative to max depth
data_max_depth <- data %>%
  group_by(ID, Date) %>%
  summarise(max_depth = min(corrected_depth))
data <- left_join(data, data_max_depth, by = c("ID","Date"))
data$rel_depth <- data$corrected_depth / data$max_depth

# Calculate distance from seabed
data$dist_from_seabed <- data$corrected_depth - data$max_depth


# Remove NA in circadian phase
data <- data[!is.na(data$night_day),]

# Remove NA in current_phase_x and current_phase_y
data <- data[!is.na(data$current_phase_x),]
# Remove NA in circadian phase
data <- data[!is.na(data$current_phase_y),]



# Summarise data per hour
#data$date_hour <- floor(data$datetime/3600)
data$date_hour <- lubridate::floor_date(data$datetime, "hour")  
data_summary <- data %>%
  group_by(ID, date_hour, night_day, current_phase_x, current_phase_y) %>%
  summarise(mean_depth = mean(corrected_depth),
            mean_rel_depth = mean(rel_depth),
            mean_seabed = mean(dist_from_seabed),
            mean_temp = mean(temperature),
            mean_moon = mean(moon_fraction),
            mean_sun_altitude = mean(sun_altitude),
            mean_sun_azimuth = mean(sun_azimuth),
            mean_direction_x = mean(direction_x),
            mean_direction_y = mean(direction_y))



# Calculate the p parallel and t transverse with 25 degrees of in the direction of the English Channel
data_summary$p_parallel <- (data_summary$mean_direction_x * cos(deg2rad(25))) + (data_summary$mean_direction_y * sin(deg2rad(25))) 
data_summary$t_transverse <- (data_summary$mean_direction_x * sin(deg2rad(25))) + (data_summary$mean_direction_y * cos(deg2rad(25))) 




### For model with relative depth from seabed as response variable ####
# Remove mean_rel_depth < 0
# This is due to depths above sea surface
plot(data_summary$mean_rel_depth)
data_summary <- filter(data_summary, mean_rel_depth > 0)

# Remove mean_rel_depth values larger than 1
data_summary <- filter(data_summary, mean_rel_depth < 1)




# Convert data to time series of either zoo class or ts class
# https://www.statology.org/r-convert-data-frame-to-time-series/

# Subset eel
#eel <- dplyr::filter(data, ID == "16031" ,
#              datetime > "2019-01-31 00:00:00" ,
#              datetime < "2019-02-13 00:00:00")
eel <- dplyr::filter(data_summary, ID == "16031" ,
              date_hour> "2019-01-31 00:00:00" ,
              date_hour < "2019-02-13 00:00:00")
eel <- dplyr::filter(data_summary, ID == "16031")

#eel <- dplyr::select(eel, datetime, rel_depth)
eel <- dplyr::select(eel, date_hour, mean_rel_depth)
eel <- na.omit(eel)
eel$ID <- NULL
eel <- eel[!duplicated(eel$date_hour), ]
class(eel)
tseries <- read.zoo(eel)
class(tseries)
tseries_ts <- as.ts(tseries)
class(tseries_ts)
tseries_ts
plot(tseries_ts,xlab='Day',ylab='Depth')
dataframe_tseries_ts <- as.data.frame(tseries_ts)

# Remove NA from time series object
tseries_ts_no_na <- na_remove(tseries_ts)
sum(is.na(tseries_ts_no_na))

# Create periodogram
# https://online.stat.psu.edu/stat510/lesson/12/12.1#:~:text=The%20raw%20periodogram%20is%20a,over%20a%20continuum%20of%20frequencies.
periodogram(tseries_ts_no_na, log = 'no', ylab='Periodogram', plot = TRUE, lwd = 3)
spec.pgram(tseries_ts_no_na, lwd = 1, xlab = 'Frequency', ylab = 'Spectrum', detrend = FALSE, spans = 6)  # 'spans' is vector of odd integers giving the widths of modified Daniell smoothers to be used to smooth the periodogram
spectrum(tseries_ts_no_na)


spec.ar(tseries_ts_no_na, log="no") # Fits an AR model to relative depth and computes the spectral density of the fitted model.
spec_values <- spec.ar(tseries_ts_no_na, log="no")

# Create dataframe
freq <- spec_values[[1]]
spec <- spec_values[[2]]

df_periodogram <- as.data.frame(cbind(freq, spec))
df_periodogram <- df_periodogram %>%
  rename(spec = V2)

# Turn frequency into hourly period
df_periodogram$freq_hour <- 1/df_periodogram$freq


# Create plot
ggplot(data=df_periodogram, aes(x=freq_hour, y=spec)) +
  geom_line() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Spectrum") +
  xlab("Hour") +
  xlim(0, 40) +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  geom_vline(xintercept = 12, linetype="solid", 
             color = "green", size=1.5) +
  geom_vline(xintercept = 24, linetype="solid", 
             color = "blue", size=1.5)




