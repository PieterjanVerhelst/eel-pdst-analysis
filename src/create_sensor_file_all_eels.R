# Create sensor log file with all eels for analysis, taking into account time settings (UTC), pressure drift and redundant data (e.g. data when on the shelf and during DVM)
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()

# Packages
library(tidyverse)
library(lubridate)



# Eel A16031####

# Read in data
eel_A16031 <- read_csv("./data/interim/sensorlogs/sensor_A16031_08-11-2019.csv")

# Aggregate data
eel_A16031$datetime <- dmy_hms(eel_A16031$datetime)
eel_A16031$datetime2 <- droplevels(cut(eel_A16031$datetime, breaks="1 min"))   # 1 min cut
eel_A16031 <- aggregate(cbind(pressure, temperature) ~ datetime2, data=eel_A16031, FUN=mean, na.rm=TRUE) 
eel_A16031$datetime2 <- ymd_hms(eel_A16031$datetime2)

# Correct for Brussels Time zone UTC + 1
eel_A16031$datetime2 <- eel_A16031$datetime2 - (60*60)
#aggdata$datetime2 <- aggdata$datetime2 - (2*60*60)  # - 2 hours when UTC+2 (summer daylight saving time)
eel_A16031$datetime2 <- as.POSIXct(eel_A16031$datetime2, "%Y-%m-%d %H:%M:%S", tz = "GMT")

# Correct for depth drift
plot(eel_A16031$datetime2, eel_A16031$pressure)
# Select date: moment of release - 15 min and pop-off moment (moment it was certainly at the surface)
subset <- filter(eel_A16031, 
                  datetime2 == as.POSIXct("2018-12-09 18:00:00", "%Y-%m-%d %H:%M:%S", tz = "GMT") |
                    datetime2 == as.POSIXct("2019-02-16 04:25:00", "%Y-%m-%d %H:%M:%S", tz = "GMT"))
plot(subset$datetime2, subset$pressure)
abline(lm(subset$pressure ~ subset$datetime2))
lm(subset$pressure ~ subset$datetime2)  # To get coefficient and estimates
# depth = (5.567e-07 * date)  -8.589e+02
eel_A16031$numericdate <- as.numeric(eel_A16031$datetime2)
eel_A16031$regression <- (5.567e-07    * eel_A16031$numericdate)   -8.589e+02
eel_A16031$corrected_depth <- eel_A16031$pressure - eel_A16031$regression

# Reverse depth
eel_A16031$corrected_depth <- eel_A16031$corrected_depth * -1

# Remove data before release and DVM part; hence, select data on continental shelf
eel_A16031 <- filter(eel_A16031, datetime2 >= "2018-12-09 18:15:00", datetime2 <= "2019-02-13 00:00:00")





# Eel A15714####

# Read in data
eel_A15714 <- read_csv("./data/interim/sensorlogs/sensor_A15714_13-02-2019.csv")

# Aggregate data
eel_A15714$datetime <- dmy_hms(eel_A15714$datetime)
eel_A15714$datetime2 <- droplevels(cut(eel_A15714$datetime, breaks="1 min"))   # 1 min cut
eel_A15714 <- aggregate(cbind(pressure, temperature) ~ datetime2, data=eel_A15714, FUN=mean, na.rm=TRUE) 
eel_A15714$datetime2 <- ymd_hms(eel_A15714$datetime2)

# Correct for Brussels Time zone UTC + 1
eel_A15714$datetime2 <- eel_A15714$datetime2 - (60*60)
#aggdata$datetime2 <- aggdata$datetime2 - (2*60*60)  # - 2 hours when UTC+2 (summer daylight saving time)
eel_A15714$datetime2 <- as.POSIXct(eel_A15714$datetime2, "%Y-%m-%d %H:%M:%S", tz = "GMT")

# Correct for depth drift
plot(eel_A15714$datetime2, eel_A15714$pressure)
# Select date: moment of release - 15 min and pop-off moment (moment it was certainly at the surface)
subset <- filter(eel_A15714, 
                 datetime2 == as.POSIXct("2018-12-04 12:45:00", "%Y-%m-%d %H:%M:%S", tz = "GMT") |
                   datetime2 == as.POSIXct("2018-12-24 07:15:00", "%Y-%m-%d %H:%M:%S", tz = "GMT"))
plot(subset$datetime2, subset$pressure)
abline(lm(subset$pressure ~ subset$datetime2))
lm(subset$pressure ~ subset$datetime2)  # To get coefficient and estimates
# depth = (7.318e-07 * date)  -1.130e+03
eel_A15714$numericdate <- as.numeric(eel_A15714$datetime2)
eel_A15714$regression <- ( 7.318e-07    * eel_A15714$numericdate)   -1.130e+03
eel_A15714$corrected_depth <- eel_A15714$pressure - eel_A15714$regression

# Reverse depth
eel_A15714$corrected_depth <- eel_A15714$corrected_depth * -1

# Remove data before release and from 1 hour before predation (2018-12-22 16:10:00) event onwards
eel_A15714 <- filter(eel_A15714, datetime2 >= "2018-12-04 13:00:00", datetime2 <= "2018-12-22 15:10:00")












# Create temperature and pressure plot from several days ####
# Create subsets of several days
subset <- filter(eel_A15714, datetime2 >= "2018-12-22 00:00:00", datetime2 <= "2018-12-23 00:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime2[1], "day"), to= subset$datetime2[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime2[1], "day"))

# Create plot
fig_subset_3days <- ggplot(subset, aes(x = datetime2,
                                       y = temperature)) +
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
  geom_vline(xintercept=gnu, color = "red", size = 1) 

fig_subset_3days


