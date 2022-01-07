# Create sensor log file with all eels for analysis, taking into account time settings (UTC), pressure drift and redundant data (e.g. data when on the shelf and during DVM)
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()

# Packages
library(tidyverse)
library(lubridate)
library(broom) # run regression per grouping variable (in 'Correct for depth drift')



# 1. Read in data ####
# Belgian eels

eel_A15805 <- read_csv("./data/interim/sensorlogs/sensor_A15805_03-04-2019.csv")
eel_A15730 <- read_csv("./data/interim/sensorlogs/sensor_A15730_15-01-2019.csv")
eel_A15757 <- read_csv("./data/interim/sensorlogs/sensor_A15757_11-12-2018.csv")
eel_A15700 <- read_csv("./data/interim/sensorlogs/sensor_A15700_10-01-2019.csv")
eel_A15714 <- read_csv("./data/interim/sensorlogs/sensor_A15714_13-02-2019.csv")
eel_A16031 <- read_csv("./data/interim/sensorlogs/sensor_A16031_08-11-2019.csv")
eel_A15706 <- read_csv("./data/interim/sensorlogs/sensor_A15706_01-08-2019.csv")
eel_A15981 <- read_csv("./data/interim/sensorlogs/sensor_A15981_06-02-2020.csv")
eel_A15777 <- read_csv("./data/interim/sensorlogs/sensor_A15777_12-11-2019.csv")
eel_A17443 <- read_csv("./data/interim/sensorlogs/sensor_A17443_27-01-2020.csv")
eel_A17499 <- read_csv("./data/interim/sensorlogs/sensor_A17499_20-02-2020.csv")
eel_A17513 <- read_csv("./data/interim/sensorlogs/sensor_A17513_20-02-2020.csv")
eel_A17534 <- read_csv("./data/interim/sensorlogs/sensor_A17534_17-03-2020.csv")
eel_A17526 <- read_csv("./data/interim/sensorlogs/sensor_A17526_17-03-2020.csv")
eel_A17522 <- read_csv("./data/interim/sensorlogs/sensor_A17522_17-03-2020.csv")
eel_A17492 <- read_csv("./data/interim/sensorlogs/sensor_A17492_05-05-2020.csv")
eel_A17508 <- read_csv("./data/interim/sensorlogs/sensor_A17508_05-05-2020.csv")
eel_A17536 <- read_csv("./data/interim/sensorlogs/sensor_A17536_05-05-2020.csv")
eel_A17538 <- read_csv("./data/interim/sensorlogs/sensor_A17538_05-05-2020.csv")
eel_A17537 <- read_csv("./data/interim/sensorlogs/sensor_A17537_05-05-2020.csv")
eel_A17510 <- read_csv("./data/interim/sensorlogs/sensor_A17510_22-06-2020.csv")
eel_A15789 <- read_csv("./data/interim/sensorlogs/sensor_A15789_22-06-2020.csv")
eel_A17521 <- read_csv("./data/interim/sensorlogs/sensor_A17521_23-09-2020.csv")
eel_A17535 <- read_csv("./data/interim/sensorlogs/sensor_A17535_09-09-2020.csv")
eel_A17653 <- read_csv("./data/interim/sensorlogs/sensor_A17653_30-12-2020.csv")
eel_A15730_2 <- read_csv("./data/interim/sensorlogs/sensor_A15730_09-03-2021.csv")
eel_A15730_2$track_tag_id <- "A15730_2"
eel_A15700_2 <- read_csv("./data/interim/sensorlogs/sensor_A15700_09-03-2021.csv")
eel_A15700_2$track_tag_id <- "A15700_2"
eel_A17646 <- read_csv("./data/interim/sensorlogs/sensor_A17646_24-01-2021.csv")
eel_A17642 <- read_csv("./data/interim/sensorlogs/sensor_A17642_11-02-2021.csv")
eel_A17658 <- read_csv("./data/interim/sensorlogs/sensor_A17658_11-02-2021.csv")
eel_A17525_2 <- read_csv("./data/interim/sensorlogs/sensor_A17525_22-02-2021.csv")
eel_A17525_2$track_tag_id <- "A17525_2"
eel_A17492_2 <- read_csv("./data/interim/sensorlogs/sensor_A17492_11-03-2021.csv")
eel_A17492_2$track_tag_id <- "A17492_2"
eel_A17518_2 <- read_csv("./data/interim/sensorlogs/sensor_A17518_11-03-2021.csv")
eel_A17518_2$track_tag_id <- "A17518_2"
eel_A17638 <- read_csv("./data/interim/sensorlogs/sensor_A17638_13-03-2021.csv")
eel_A17634 <- read_csv("./data/interim/sensorlogs/sensor_A17634_19-03-2021.csv")
eel_A17547 <- read_csv("./data/interim/sensorlogs/sensor_A17547_19-03-2021.csv")
eel_A17635 <- read_csv("./data/interim/sensorlogs/sensor_A17635_14-04-2021.csv")
eel_A17487 <- read_csv("./data/interim/sensorlogs/sensor_A17487_15-04-2021.csv")
eel_A17499_2 <- read_csv("./data/interim/sensorlogs/sensor_A17499_15-04-2021.csv")
eel_A17499_2$track_tag_id <- "eel_A17499_2"
eel_A17663 <- read_csv("./data/interim/sensorlogs/sensor_A17663_14-05-2021.csv")
eel_A17513_2 <- read_csv("./data/interim/sensorlogs/sensor_A17513_17-05-2021.csv")
eel_A17513_2$track_tag_id <- "eel_A17513_2"
eel_A17648 <- read_csv("./data/interim/sensorlogs/sensor_A17648_25-06-2021.csv")


# Combine all datasets ####
all <- do.call("rbind", list(eel_A15805,
                             eel_A15730,
                             eel_A15757,
                             eel_A15700,
                             eel_A15714,
                             eel_A16031,
                             eel_A15706,
                             eel_A15981,
                             eel_A15777,
                             eel_A17443,
                             eel_A17499,
                             eel_A17513,
                             eel_A17534,
                             eel_A17526,
                             eel_A17522,
                             eel_A17492,
                             eel_A17508,
                             eel_A17536,
                             eel_A17538,
                             eel_A17537,
                             eel_A17510,
                             eel_A15789,
                             eel_A17521,
                             eel_A17535,
                             eel_A17653,
                             eel_A15730_2,
                             eel_A15700_2,
                             eel_A17646,
                             eel_A17642,
                             eel_A17658,
                             eel_A17525_2,
                             eel_A17492_2,
                             eel_A17518_2,
                             eel_A17638,
                             eel_A17634,
                             eel_A17547,
                             eel_A17635,
                             eel_A17487,
                             eel_A17499_2,
                             eel_A17663,
                             eel_A17513_2,
                             eel_A17648
))

all <- all %>%
  rename(ID = track_tag_id)
all$ID <- factor(all$ID)

# Remove seperate files
rm(eel_A15805,
   eel_A15730,
   eel_A15757,
   eel_A15700,
   eel_A15714,
   eel_A16031,
   eel_A15706,
   eel_A15981,
   eel_A15777,
   eel_A17443,
   eel_A17499,
   eel_A17513,
   eel_A17534,
   eel_A17526,
   eel_A17522,
   eel_A17492,
   eel_A17508,
   eel_A17536,
   eel_A17538,
   eel_A17537,
   eel_A17510,
   eel_A15789,
   eel_A17521,
   eel_A17535,
   eel_A17653,
   eel_A15730_2,
   eel_A15700_2,
   eel_A17646,
   eel_A17642,
   eel_A17658,
   eel_A17525_2,
   eel_A17492_2,
   eel_A17518_2,
   eel_A17638,
   eel_A17634,
   eel_A17547,
   eel_A17635,
   eel_A17487,
   eel_A17499_2,
   eel_A17663,
   eel_A17513_2,
   eel_A17648)


# 2. Read in parameter file ####
parameters <- read_csv("./data/external/parameters.csv")
parameters$start_datetime <-  dmy_hm(parameters$start_datetime)
parameters$end_datetime <-  dmy_hm(parameters$end_datetime)
parameters$bank_datetime <-  dmy_hm(parameters$bank_datetime)
parameters$popoff_datetime <-  dmy_hm(parameters$popoff_datetime)
#parameters$popoff_datetime15 <-  parameters$popoff_datetime + minutes(15)
parameters$UTC <-  factor(parameters$UTC)


# Temporarily remove 2 PSAT eels
#parameters <- parameters %>%
#  filter(ID != c('112061')) %>%
#  filter(ID != c('112064'))



# 3. Time zone correction ####
#all <- left_join(all, parameters, by = "ID") %>%
#  mutate(datetime = ifelse(UTC == "-1", (datetime - (60*60)), 
#                            ifelse(UTC == "-2", datetime - (2*60*60))))
#all$datetime <- as.POSIXct(all$datetime, origin='1970-01-01 00:00:00')
#all$time_diff <- all$datetime2 - all$datetime    # Check for time zone correction

# All Belgian eels in same time zone, so simply datetime - 1 hour for all data
all$datetime <- dmy_hms(all$datetime)
all$datetime_local_zone <- all$datetime
all$datetime <- all$datetime - 3600

# Check for time zone correction
all$time_diff <- all$datetime_local_zone - all$datetime
unique(all$time_diff)
all$time_diff <- NULL             # Remove redunant column
all$datetime_local_zone <- NULL   # Remove redunant column


# 4. Correct for depth drift ####

# Select rows with bank datetime and popoff datetime + 15 minutes (= when tag was at atmospheric pressure)
all <- left_join(all, parameters, by = "ID")
bank_popoff <- all %>%
  group_by(ID) %>%
  filter(datetime == bank_datetime | datetime == popoff_datetime,
         pressure_correction != 0) %>%
  mutate(numericdate = as.numeric(datetime))

# Apply linear regression per ID
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
#model <- bank_popoff %>%
#  group_by(ID) %>%
#  do(fit_model = lm(pressure ~ datetime, data = .)) 
model <- bank_popoff %>%
  nest(data = -ID) %>% 
  mutate(
    fit = map(data, ~ lm(pressure ~ datetime, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)

# Remove redundant columns
model <- dplyr::select(model, -data, -fit, -std.error, -statistic, -p.value)


# Extract model coefficients in a tidy dataframe
#model_coef <- tidy(model, fit_model) %>%
#  spread(key = term, value = estimate) %>%
#  select(-std.error, -statistic, -p.value) %>%
#  rename(intercept_val = `(Intercept)`,
#         datetime_val = datetime)

model_coef <- model %>%
  spread(key = term, value = estimate) %>%
  rename(intercept_val = `(Intercept)`,
         datetime_val = datetime)



# Join values to dataset
all <- left_join(all, model_coef, by = "ID")

# Calculate corrected depth
all$numericdate <- as.numeric(all$datetime)
all$regression <- (all$datetime_val  * all$numericdate)  + all$intercept_val
all$corrected_depth <- all$pressure - all$regression

# For some eels, no correction could be applied. Hence, take original pressure data.
all <- all %>%
  mutate(corrected_depth2 = coalesce(corrected_depth, pressure))

# Reverse depth
all$corrected_depth2 <- all$corrected_depth2 * -1






# 5. Aggregate data per 5 min  or 60 min ####
#all <- all %>%
#  group_by(ID) %>%
#  fill(temperature) %>%   # Fill temperature NA's with previous measured value
#  mutate(datetime = dmy_hms(datetime))

#all$datetime2 <- droplevels(cut(all$datetime, breaks="5 min"))   # 5 min cut
#all$datetime2 <- droplevels(cut(all$datetime, breaks="60 min"))   # 60 min cut

#all <- all %>%
#  group_by(ID, datetime2) %>%
#  summarise(pressure = mean(pressure),            # Calculate mean pressure
#            temperature = mean(temperature))      # Calculate mean temperature

#all$datetime2 <- ymd_hms(all$datetime2)



# 5. Subsample data per 5 min ####
all <- all %>% group_by(ID) %>%
  slice(seq(1, n(), by = 150))



# 6. Remove data before release and
# - till DVM
# - till one hour before predation
# - 15 minutes before popoff time
# --> Hence, select data on continental shelf
all <- all %>%
  group_by(ID) %>%
  filter(datetime >= start_datetime, datetime <= end_datetime)


# 6b. Remove data before release and after pop-off
#all <- all %>%
#  group_by(ID) %>%
#  filter(datetime >= start_datetime, datetime <= popoff_datetime)



# Check depth profiles before and after correction
#ind_eel <- filter(all, ID == "A16031")

#plot(ind_eel$datetime, ind_eel$pressure)
#plot(ind_eel$datetime, ind_eel$corrected_depth2)



# 7. Clean dataset  ####
all <- all %>%
  dplyr::select(ID, datetime, numericdate, corrected_depth2, temperature) %>%
  rename(corrected_depth = corrected_depth2)


# 8. Merge eel metadata to dataset
eel <- read.csv("./data/external/eel_metadata.csv")
eel$ID <- factor(eel$ID)
eel$Direction <- factor(eel$Direction)
eel$Country <- factor(eel$Country)

all <- left_join(all, eel, by = "ID")


# 9. Create column 'Date' and remove ID prefix
all$Date <- as.Date(all$datetime)
all$ID <- gsub( "A0", "", as.character(all$ID))
all$ID <- gsub( "A", "", as.character(all$ID))
all$ID <- factor(all$ID)


# 10. Load trajectory dataset with coordinates ####
tr_data <-  list.files(path = "./data/external/trajectory_data/",
                       pattern = "*.csv", 
                       full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

# Select columns
tr_data <- dplyr::select(tr_data, ID, Date, MPL.Avg.Lat, MPL.Avg.Lon, Med.Lat, Med.Lon)
tr_data <- rename(tr_data, 
                  geoloc_avg_lat = MPL.Avg.Lat,
                  geoloc_avg_lon = MPL.Avg.Lon,
                  geoloc_med_lat = Med.Lat,
                  geoloc_med_lon = Med.Lon)

# Process columns
tr_data$Date <- as.Date(tr_data$Date)
tr_data$ID <- factor(tr_data$ID)

# Remove double dates per eel (ID)
#tr_data <- tr_data[!duplicated(tr_data[c('ID','Date')]),]
tr_data <- tr_data %>%     # Add ID number to duplicate dates
  group_by(ID, Date) %>%
  add_tally()

duplicates <- filter(tr_data, n == 2)   # Filter duplicate dates
duplicates <- duplicates %>%             # Add ID number to distinguish between first and second duplicate
  mutate(number_id = row_number())
duplicates <- filter(duplicates, number_id == 2)  # Filter second duplicates

tr_data <- filter(tr_data, n != 2)   # Remove duplicate dates from tracking dataset

# Bind 'duplicates' dataset with second duplicates to tracking dataset
tr_data <- ungroup(tr_data)
tr_data$n <- NULL
duplicates <- ungroup(duplicates)
duplicates$n <- NULL
duplicates$number_id <- NULL

tr_data <- rbind(tr_data, duplicates)


# Select relevant eels
all <- filter(all, ID == "9359" |
                 ID == "15714" |
                 ID == "15777" |
                 ID == "16031" |
                 ID == "15706" |
                 ID == "17443" |
                 ID == "17499" |
                 ID == "17513" |
                 ID == "9349" |
                 ID == "9358" |
                 ID == "9374" |
                 ID == "9377" |
                 ID == "9393" |
                 ID == "9411" |
                 ID == "9423" |
                 ID == "9424" |
                 ID == "17534" |
                 ID == "17526" |
                 ID == "17522" |
                 ID == "17492" |
                 ID == "17508" |
                 ID == "17536" |
                 ID == "17538" |
                 ID == "17537" |
                 ID == "17510" |
                 ID == "15789")
all$ID <- factor(all$ID) # rerun 'factor()' so the number of levels is set accurately

tr_data <- filter(tr_data, ID == "9359" |
                    ID == "15714" |
                    ID == "15777" |
                    ID == "16031" |
                    ID == "15706" |
                    ID == "17443" |
                    ID == "17499" |
                    ID == "17513" |
                    ID == "9349" |
                    ID == "9358" |
                    ID == "9374" |
                    ID == "9377" |
                    ID == "9393" |
                    ID == "9411" |
                    ID == "9423" |
                    ID == "9424" |
                    ID == "17534" |
                    ID == "17526" |
                    ID == "17522" |
                    ID == "17492" |
                    ID == "17508" |
                    ID == "17536" |
                    ID == "17538" |
                    ID == "17537" |
                    ID == "17510" |
                    ID == "15789")
tr_data$ID <- factor(tr_data$ID) # rerun 'factor()' so the number of levels is set accurately


# 11. Merge datasets ####
all <- left_join(x = all, y = tr_data, by=c("ID","Date"))
all$geoloc_avg_lat <- as.numeric(all$geoloc_avg_lat)  # important to convert lat and lon to numeric for getSunlightTimes()
all$geoloc_avg_lon <- as.numeric(all$geoloc_avg_lon)
#plot(all$geoloc_avg_lon, all$geoloc_avg_lat)




# 12. Write csv file
#write.csv(all, "./data/interim/batch_processed_eels_5min.csv")
#write.csv(all, "./data/interim/batch_processed_eels_1hour.csv")
#write.csv(all, "./data/interim/batch_processed_eels_5min_totaltrack.csv")



