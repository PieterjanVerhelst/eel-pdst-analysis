# Create input file for model: temperature and pressure (depth) files
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



# Packages
library(tidyverse)
library(lubridate)

# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()

# 1. Read in sensor data ####
sensordata <- read_csv("./data/interim/sensor_A09359_11-12-2012.csv")


# 2. Aggregate data ####
# Hence, make sure temperature and depth data are of equal length
sensordata$datetime <- dmy_hms(sensordata$datetime)
sensordata$datetime2 <- droplevels(cut(sensordata$datetime, breaks="5 min"))   # 5 min cut

aggdata <- aggregate(cbind(pressure, temperature) ~ datetime2, data=sensordata, FUN=mean, na.rm=TRUE)
aggdata$datetime2 <- ymd_hms(aggdata$datetime2)

# Correct for Brussels Time zone UTC + 1
#aggdata$datetime2 <- aggdata$datetime2 - (60*60)
aggdata$datetime2 <- aggdata$datetime2 - (2*60*60)  # - 2 hours when UTC+2 (summer daylight saving time)
aggdata$datetime2 <- as.POSIXct(aggdata$datetime2, "%Y-%m-%d %H:%M:%S", tz = "GMT")


# Reverse depth
#aggdata$pressure <- aggdata$pressure * -1   # Not needed

# Set release and retrieval or pop off time (midday following popping event)
# ! Make sure resease is in UTC instead of UTC+1 !
release <- as.POSIXct("2012-09-27 12:40:00", "%Y-%m-%d %H:%M:%S", tz = "GMT")
retrieval <- as.POSIXct("2012-11-16 23:55:00", "%Y-%m-%d %H:%M:%S", tz = "GMT") # Take day before retrieval, since exact moment of retrieval is unknown


# 3. Subset from release to retrieval date ####
# ! Check if release time is correct related to UTC vs UTC+1 !
subset <- filter(aggdata, datetime2 >= release, datetime2 <= retrieval)

# 4. Select temperature data only ####
temp <- select(subset, datetime2, temperature)
colnames(temp)[1] <- "Date"
colnames(temp)[2] <- "Temp"

# Set in correct date-time format dd/mm/yy HH:MM
#temp$Date <- format(as.POSIXct(temp$Date, format = "%y%m%d %H:%M:%S"), "%d/%m/%Y %H:%M:%S")  # with seconds
#temp$Date <- format(as.POSIXct(temp$Date2, format = "%y%m%d %H:%M:%S"), "%d/%m/%Y %H:%M")  # without seconds

# Arrange temp ataset
#temp$Date2 <- NULL
#temp <- select(temp, Date, Temp)


# 5. Select pressure data only ####
press <- select(subset, datetime2, pressure)
colnames(press)[1] <- "Date"
colnames(press)[2] <- "Depth"

# Set in correct date-time format dd/mm/yy HH:MM
# Note that the correct date time format is stored in a different column. The original column (yyyy-mm-dd hh:mm:ss) is needed for drift correctino (see further)
#temp$Date2 <- format(as.POSIXct(temp$Date, format = "%y%m%d %H:%M:%S"), "%d/%m/%Y %H:%M:%S")  # with seconds
#press$Date <- format(as.POSIXct(press$Date2, format = "%y%m%d %H:%M:%S"), "%d/%m/%Y %H:%M")  # without seconds


# 6. Correct for pressure sensor drift ####
plot(press$Date, press$Depth)
# Select date: moment of release - 15 min and pop-off moment (moment it was certainly at the surface)
subset2 <- filter(aggdata, 
                  datetime2 == as.POSIXct("2012-09-27 12:25:00", "%Y-%m-%d %H:%M:%S", tz = "GMT") |
                  datetime2 == as.POSIXct("2012-10-18 19:00:00", "%Y-%m-%d %H:%M:%S", tz = "GMT"))
plot(subset2$datetime2, subset2$pressure)
abline(lm(subset2$pressure ~ subset2$datetime2))
lm(subset2$pressure ~ subset2$datetime2)  # To get coefficient and estimates
# depth = (2.322e-05 * date)  -3.587e+04

press$numericdate <- as.numeric(press$Date)
press$regression <- (   1.124e-07   *press$numericdate)    -1.538e+02 
press$corrected_depth <- press$Depth-press$regression


# Some check diagnostics
press$diff <- press$Depth-press$corrected_depth
plot(press$Date, press$diff)
plot(press$Date, press$Depth)
plot(press$Date, press$corrected_depth)
check <- filter(press, Date >= "2019-11-02 18:15:00", Date <= "2019-11-28 10:00:00")
summary(check)



# rearrange pressure file
#press$Date2 <- NULL
press$Depth <- NULL
press$numericdate <- NULL
press$regression <- NULL
press$diff <- NULL
press <- rename(press, Depth = corrected_depth)


# 7. Write csv files ####
write.csv(temp, "./data/interim/input_A09359/EELA09359TEMP.csv", row.names = FALSE)
write.csv(press, "./data/interim/input_A09359/EELA09359PRES.csv", row.names = FALSE)




