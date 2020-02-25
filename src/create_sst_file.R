# Create file with daily sea surface temperatures (first 20 m depth) and max depths
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)


# 1. Read in temperature and corrected pressure data ####
temp_data <- read_csv("./data/interim/input_A16031/EELA16031TEMP.csv")
press_data <- read_csv("./data/interim/input_A16031/EELA16031PRES.csv")

# Merge them together
temp_press <- merge(temp_data, press_data, by="Date")


# Set release and retrieval to create plots
release <- "2018-12-09 19:15:00"
retrieval <- "2019-03-24 12:00:00"


# 2. Subset from release to retrieval date ####
# Note for first day, take only the values since release
subset <- filter(temp_press , Date >= release, Date <= "2018-12-09 23:55:00")

subset <- filter(temp_press , Date >= "2019-03-24 00:00:00", Date <= "2019-03-24 23:55:00")

# summary to check max depth
summary(subset)

# max depth
max(subset$Depth)

# average temp
mean(subset$Temp)

# In case depth > 20 m, create subset to calculate sea surface temperature
subset_sst <- filter(subset, Depth <= 20)
mean(subset_sst$Temp)



# 3. Create file and fill in values manually ####
input_sst <- data.frame(seq(as.Date("2018/12/09"), as.Date("2019/03/24"), "days"))
colnames(input_sst)[1] <- "Date/Time Stamp"

input_sst$Temp <- NA
input_sst$SST_depth <- NA
input_sst$Max_Pressure <- NA


# Run in values manually
i = 106

input_sst$Temp[i] <-   mean(subset_sst$Temp)
input_sst$SST_depth[i] <- 0
input_sst$Max_Pressure[i] <- max(subset$Depth)



# 4. Write csv files ####
write.csv(input_sst, "./data/interim/input_A16031/EELA16031TEMP_F.csv", row.names = FALSE)





