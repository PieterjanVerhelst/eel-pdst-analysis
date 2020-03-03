# Create file with daily sea surface temperatures (first 20 m depth) and max depths
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Packages
library(tidyverse)
library(lubridate)

# Set time zone
Sys.setenv(TZ='GMT')

# 1. Read in temperature and corrected pressure data ####
temp_data <- read_csv("./data/interim/input_A15706/EELA15706TEMP.csv")
press_data <- read_csv("./data/interim/input_A15706/EELA15706PRES.csv")


# Merge them together
temp_press <- merge(temp_data, press_data, by="Date")


# Set date as POSIXct
# temp_press$Date <- as.POSIXct(temp_press$Date, format = "%d/%m/%Y %H:%M")
temp_press$Date <- ymd_hms(temp$Date)
temp_press$Date <- as.POSIXct(temp$Date, "%Y-%m-%d %H:%M:%S", tz = "GMT")


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
colnames(input_sst)[1] <- "Date/Time Stamp"
input_sst <- input_sst[,c(1,3,4,2)]


# 4. Write csv files ####
write.csv(input_sst, "./data/interim/input_A15706/EELA15706TEMP_F.csv", na = "NaN", row.names = FALSE)


# Check some diagnostics

subset <- filter(temp_press, Date >= "2019-04-05 00:00:00", Date <= "2019-04-05 23:55:00")
max(subset$Depth)
mean(subset$Temp)















