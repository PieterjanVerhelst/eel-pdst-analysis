# Create plots of sensordata obtained by pDST
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)


# 1. Read in sensor data ####
sensordata <- read_csv("./data/interim/sensor_A16031_08-11-2019.csv")


# 2. Aggregate data ####
sensordata$datetime <- dmy_hms(sensordata$datetime)
#sensordata$datetime2 <- droplevels(cut(sensordata$datetime, breaks="hour"))   # 1 hour cut
sensordata$datetime2 <- droplevels(cut(sensordata$datetime, breaks="5 min"))   # 5 min cut

aggdata <- aggregate(cbind(pressure, temperature) ~ datetime2, data=sensordata, FUN=mean, na.rm=TRUE) 
aggdata$datetime2 <- ymd_hms(aggdata$datetime2)

# Reverse depth
aggdata$pressure <- aggdata$pressure * -1


# 3. Set release and retrieval to create plots ####
release <- "2018-12-09 19:15:00"
retrieval <- "2019-03-24 12:00:00"


# 4. Create temperature and pressure plot for total dataset ####
fig_t_p <- ggplot(aggdata, aes(x = datetime2,
                               y = temperature)) +
  geom_point(binaxis='x', dotsize=0.5, binwidth = 1) +
  geom_point(data = aggdata, aes(x = datetime2, y = pressure/2), dotsize = 0.5, alpha = 0.5, colour = "purple") +
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
subset <- filter(aggdata, datetime2 >= as.Date(release)-1, datetime2 <= as.Date(retrieval)+1)

fig_subset <- ggplot(subset, aes(x = datetime2,
                                 y = temperature)) +
  geom_point(binaxis='x', dotsize=0.5, binwidth = 1) +
  geom_point(data = subset, aes(x = datetime2, y = pressure/2), dotsize = 0.5, alpha = 0.5, colour = "purple") +
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

fig_subset


# 6. Create temperature and pressure plot from several days ####
# Create subsets of several days
subset <- filter(aggdata, datetime2 >= "2019-01-14 01:00:00", datetime2 <= "2019-01-15 01:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime2[1], "day"), to= subset$datetime2[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime2[1], "day"))

# Create plot
fig_subset_3days <- ggplot(subset, aes(x = datetime2,
                                   y = temperature)) +
  geom_point(binaxis='x', dotsize=0.5, binwidth = 1) +
  geom_point(data = subset, aes(x = datetime2, y = pressure/2), dotsize = 0.5, alpha = 0.5, colour = "purple") +
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

