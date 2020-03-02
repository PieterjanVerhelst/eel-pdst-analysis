# Create file with daily sea surface temperatures (first 20 m depth) and max depths
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)


# 1. Read in temperature and corrected pressure data ####
temp_data <- read_csv("./data/interim/input_A15700/EELA15700TEMP.csv")
press_data <- read_csv("./data/interim/input_A15700/EELA15700PRES.csv")


# Merge them together
temp_press <- merge(temp_data, press_data, by="Date")


# Set date as POSIXct
temp_press$Date <- as.POSIXct(temp_press$Date, format = "%d/%m/%Y %H:%M")


# 2. Calculate mean temperature for top 20 m water layer and total max depth ####
input_sst <- temp_press %>%
  group_by(as.Date(Date)) %>%
  summarize(Max_Pressure = max(Depth),
            Temp = mean(Temp[Depth<=20]))


# 3. Add column to flag if an eel swam at the top 20 m water layer (if not, indicate by 'NaN') ####
input_sst$SST_depth <- NA

for (i in 1:dim(input_sst)[1]){
  if (input_sst$Temp[i] == 'NaN'){
    input_sst$SST_depth[i] = NaN
  } else{
    input_sst$SST_depth[i] = 0
  }}




# Arrange dataset
colnames(input_sst)[1] <- "Date"

# Set in correct date-time format dd/mm/yy HH:MM
input_sst$Date <- format(as.POSIXct(input_sst$Date, format = "%y%m%d"), "%d/%m/%Y")  # without seconds


# Arrange dataset
colnames(input_sst)[1] <- "Date/Time Stamp"
input_sst <- input_sst[,c(1,3,4,2)]


# 4. Write csv files ####
write.csv(input_sst, "./data/interim/input_A17528/EELA17528TEMP_F.csv", na = "NaN", row.names = FALSE)




subset <- filter(aggdata, datetime2 >= "2018-11-23 00:00:00", datetime2 <= "2018-11-23 23:55:00")
max(subset$pressure)
mean(subset$pressure)














