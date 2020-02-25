# Create file with daily sea surface temperatures (first 20 m depth) and max depths
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


# Set release and retrieval to create plots
release <- "2018-12-09 19:15:00"
retrieval <- "2019-03-24 12:00:00"


# 3. Subset from release to retrieval date ####
# Note for first day, take only the values since release
subset <- filter(aggdata, datetime2 >= release, datetime2 <= "2018-12-09 23:55:00")

subset <- filter(aggdata, datetime2 >= "2019-03-24 00:00:00", datetime2 <= "2019-03-24 23:55:00")

# summary to check max depth
summary(subset)

# max depth
max(subset$pressure)

# average temp
mean(subset$temperature)

# In case depth > 20 m, create subset to calculate sea surface temperature
subset_sst <- filter(subset, pressure <= 20)
mean(subset_sst$temperature)



# 4. Create file and fill in values manually ####
input_sst <- data.frame(seq(as.Date("2018/12/09"), as.Date("2019/03/24"), "days"))
colnames(input_sst)[1] <- "Date/Time Stamp"

input_sst$Temp <- NA
input_sst$SST_depth <- NA
input_sst$Max_Pressure <- NA


# Run in values manually
i = 106

input_sst$Temp[i] <-   15.51303
input_sst$SST_depth[i] <- 0
input_sst$Max_Pressure[i] <- 8.145



# 5. Write csv files ####
write.csv(input_sst, "./data/interim/input_A16031/EELA16031TEMP_F.csv", row.names = FALSE)





