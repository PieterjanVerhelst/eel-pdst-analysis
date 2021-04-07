# Create plots of sensordata obtained by pDST
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)

# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()


# 1. Read in sensor data ####
sensordata <- read_csv("./data/interim/sensorlogs/sensor_A17443_27-01-2020.csv")


# 2. Aggregate data ####
#sensordata$datetime <- dmy_hms(sensordata$datetime)
#sensordata$datetime <- droplevels(cut(sensordata$datetime, breaks="hour"))   # 1 hour cut
#sensordata$datetime <- droplevels(cut(sensordata$datetime, breaks="5 min"))   # 5 min cut

#aggdata <- aggregate(cbind(pressure, temperature) ~ datetime, data=sensordata, FUN=mean, na.rm=TRUE) 
#aggdata$datetime <- ymd_hms(aggdata$datetime)


# 2. Subsample data
sensordata$datetime <- dmy_hms(sensordata$datetime)
aggdata <- sensordata[seq(1, nrow(sensordata), 30), ]
aggdata$track_tag_id <- NULL


# Correct for Brussels Time zone UTC + 1
aggdata$datetime <- aggdata$datetime - (60*60)
#aggdata$datetime <- aggdata$datetime - (2*60*60)  # - 2 hours when UTC+2 (summer daylight saving time)
aggdata$datetime <- as.POSIXct(aggdata$datetime, "%Y-%m-%d %H:%M:%S", tz = "GMT")

# Reverse depth
aggdata$pressure <- aggdata$pressure * -1


# 3. Set release and retrieval to create plots ####
# For retrieval, take day before retrieval at 23:55
# Note to put release date in UTC!
release <- as.POSIXct("2018-10-31 18:44:00", "%Y-%m-%d %H:%M:%S", tz = "GMT")
retrieval <- as.POSIXct("2018-12-13 23:59:00", "%Y-%m-%d %H:%M:%S", tz = "GMT") # Take day before retrieval, since exact moment of retrieval is unknown
pop <- as.POSIXct("2018-11-06 16:35:00", "%Y-%m-%d %H:%M:%S", tz = "GMT")


# 4. Create temperature and pressure plot for total dataset ####
fig_t_p <- ggplot(aggdata, aes(x = datetime,
                               y = temperature)) +
  geom_line(binaxis='x', size=1.0, binwidth = 1) +
  geom_line(data = aggdata, aes(x = datetime, y = pressure/2), size = 1.0, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Pressure (m)")) +
  theme_minimal() +
  ylab("Temperature (°C)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="5 days") + 
  geom_vline(xintercept=ymd_hms(release), colour="blue") +  # Release date and time
  geom_vline(xintercept=ymd_hms(retrieval), colour="red") +  # Retrieval date
  geom_text(aes(x=ymd_hms(release), label="Release", y=18), colour="blue", angle=90, vjust = 1.2, text=element_text(size=11)) + 
  geom_text(aes(x=ymd_hms(retrieval), label="Retrieval", y=18), colour="red", angle=90, vjust = 1.2, text=element_text(size=11))

fig_t_p



# 5. Create temperature and pressure plot from release to retrieval date ####
subset <- filter(aggdata, datetime >= as.Date(release)-1, datetime <= as.Date(retrieval)+1)

fig_rel_ret <- ggplot(subset, aes(x = datetime,
                               y = temperature)) +
  geom_line(binaxis='x', size=1.0, binwidth = 1) +
  geom_line(data = subset, aes(x = datetime, y = pressure/2), size = 1.0, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Pressure (m)")) +
  theme_minimal() +
  ylab("Temperature (°C)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="3 days") + 
  geom_vline(xintercept=ymd_hms(release), colour="blue") +  # Release date and time
  geom_vline(xintercept=ymd_hms(retrieval), colour="red") +  # Retrieval date
  geom_text(aes(x=ymd_hms(release), label="Release", y=18), colour="blue", angle=90, vjust = 1.2, text=element_text(size=11)) + 
  geom_text(aes(x=ymd_hms(retrieval), label="Retrieval", y=18), colour="red", angle=90, vjust = 1.2, text=element_text(size=11))

fig_rel_ret




# 6. Create temperature and pressure plot from release to pop-off date ####
subset <- filter(aggdata, datetime >= as.Date(release)-1, datetime <= as.Date(pop)+1)

fig_rel_pop <- ggplot(subset, aes(x = datetime,
                                  y = temperature)) +
  geom_line(binaxis='x', size=1.0, binwidth = 1) +
  geom_line(data = subset, aes(x = datetime, y = pressure/2), size = 1.0, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Pressure (m)")) +
  theme_minimal() +
  ylab("Temperature (°C)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 day") + 
  geom_vline(xintercept=ymd_hms(release), colour="blue") +  # Release date and time
  geom_vline(xintercept=ymd_hms(pop), colour="green") +  # Retrieval date
  geom_text(aes(x=ymd_hms(release), label="Release", y=18), colour="blue", angle=90, vjust = 1.2, text=element_text(size=11)) + 
  geom_text(aes(x=ymd_hms(pop), label="Pop-off", y=18), colour="green", angle=90, vjust = 1.2, text=element_text(size=11))

fig_rel_pop




# 7. Create temperature and pressure plot from several days ####
# Create subsets of several days
subset <- filter(aggdata, datetime >= "2019-11-28 07:00:00", datetime <= "2019-11-28 14:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime[1], "day"), to= subset$datetime[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime[1], "day"))

# Create plot
fig_subset_3days <- ggplot(subset, aes(x = datetime,
                                   y = temperature)) +
  geom_line(binaxis='x', size=1.0, binwidth = 1) +
  geom_line(data = subset, aes(x = datetime, y = pressure/2), size = 1.0, alpha = 0.5, colour = "purple") +
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
  geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) 

fig_subset_3days

