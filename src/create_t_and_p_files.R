# Create input file for model: temperature and pressure (depth) files
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



# Packages
library(tidyverse)
library(lubridate)


# 1. Read in sensor data ####
sensordata <- read_csv("./data/interim/sensor_A15714_13-02-2019.csv")


# 2. Aggregate data ####
# Hence, make sure temperature and depth data are of equal length
sensordata$datetime <- dmy_hms(sensordata$datetime)
sensordata$datetime2 <- droplevels(cut(sensordata$datetime, breaks="5 min"))   # 5 min cut

aggdata <- aggregate(cbind(pressure, temperature) ~ datetime2, data=sensordata, FUN=mean, na.rm=TRUE) 
aggdata$datetime2 <- ymd_hms(aggdata$datetime2)

# Reverse depth
#aggdata$pressure <- aggdata$pressure * -1   # Not needed

# Set release and retrieval to create plots
release <- "2018-12-04 13:55:00"
retrieval <- "2019-01-22 12:00:00"

# 3. Subset from release to retrieval date ####
subset <- filter(aggdata, datetime2 >= release, datetime2 <= retrieval)

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
# Select date: moment of release and day of retrieval
subset2 <- filter(press, Date == release | Date == "2019-01-22 00:00:00")
plot(subset2$Date, subset2$Depth)
abline(lm(subset2$Depth ~ subset2$Date))
lm(subset2$Depth ~ subset2$Date)  # To get coefficient and estimates
# depth = (7.628e-07 * date)  -1.177e+03

press$numericdate <- as.numeric(press$Date)
press$regression <- (7.628e-07 *press$numericdate)   -1.177e+03
press$corrected_depth <- press$Depth-press$regression


# Some check diagnostics
press$diff <- press$Depth-press$corrected_depth
plot(press$Date, press$diff)
plot(press$Date, press$Depth)
plot(press$Date, press$corrected_depth)
check <- filter(press, Date >= "2019-11-02 18:15:00", Date <= "2019-11-28 10:00:00")
summary(check)


# rearrange pressure file
press$Depth <- NULL
press$numericdate <- NULL
press$regression <- NULL
press$diff <- NULL
press <- rename(press, Depth = corrected_depth)



# 7. Write csv files ####
write.csv(temp, "./data/interim/input_A15714/EELA15714TEMP.csv", row.names = FALSE)
write.csv(press, "./data/interim/input_A15714/EELA15714PRES.csv", row.names = FALSE)




