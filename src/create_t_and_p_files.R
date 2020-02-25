# Create input file for model: temperature and pressure (depth) files
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



# Packages
library(tidyverse)
library(lubridate)


# 1. Read in sensor data ####
sensordata <- read_csv("./data/interim/sensor_A16031_08-11-2019.csv")


# 2. Aggregate data ####
# Hence, make sure temperature and depth data are of equal length
sensordata$datetime <- dmy_hms(sensordata$datetime)
sensordata$datetime2 <- droplevels(cut(sensordata$datetime, breaks="5 min"))   # 5 min cut

aggdata <- aggregate(cbind(pressure, temperature) ~ datetime2, data=sensordata, FUN=mean, na.rm=TRUE) 
aggdata$datetime2 <- ymd_hms(aggdata$datetime2)

# Reverse depth
#aggdata$pressure <- aggdata$pressure * -1   # Not needed


# 3. Subset from release to retrieval date ####
subset <- filter(aggdata, datetime2 >= as.Date(release)-1, datetime2 <= as.Date(retrieval))

# 4. Select temperature data only ####
temp <- select(subset, datetime2, temperature)
colnames(temp)[1] <- "Date"
colnames(temp)[2] <- "Temp"

# 5. Select pressure data only ####
press <- select(subset, datetime2, pressure)
colnames(press)[1] <- "Date"
colnames(press)[2] <- "Depth"

# 6. Correct for pressure sensor drift ####
plot(press$Date, press$Depth)
subset2 <- filter(press, Date == "2018-12-09 11:00:00" | Date >= "2019-02-18 00:00:00", Date <= "2019-03-04 00:00:00")
plot(subset2$Date, subset2$Depth)
abline(lm(subset2$Depth ~ subset2$Date))
lm(subset2$Depth ~ subset2$Date)  # To get coefficient and estimates
# Depth <- (4.481e-07 * Date)  -6.925e+02



# 7. Write csv files ####
write.csv(temp, "./data/interim/input_A16031/EELA16031TEMP.csv", row.names = FALSE)
write.csv(press, "./data/interim/input_A16031/EELA16031PRES.csv", row.names = FALSE)




