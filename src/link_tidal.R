# Link tidal data* to the dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be
# *tidal data obtained via John Aldridge (CEFAS, UK. john.aldridge@cefas.co.uk)

# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()


# Packages
library(tidyverse)
library(lubridate)




# 1. Load dataset with all eels ####
data <- read_csv("./data/interim/data_circadian_5min.csv")

data$...1 <- NULL
data$ID <- factor(data$ID)


# 2. Load tidal data ####

# Load old tidal data with both 2018 and 2019 data
# Remove 2018 data that is not correct
tidal_list_names <- list.files(path = "./data/external/tidal_old_files/",
                               pattern = "*.dat", 
                               full.names = T)

tidal_list <- lapply(tidal_list_names, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

tidal <- data.table::rbindlist(tidal_list)


tidal$V1 <- dmy(tidal$V1)
tidal$year <- year(tidal$V1)
tidal <- filter(tidal, year == 2019)

tidal$source_file <- str_replace(tidal$source_file, "./data/external/tidal_old_files/eel_", "") 
tidal$source_file <- str_replace(tidal$source_file, ".dat", "")


# Load correct 2018 data
tidal_list_names <- list.files(path = "./data/external/tidal_2018/",
                               pattern = "*.dat", 
                               full.names = T)

tidal_list <- lapply(tidal_list_names, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

tidal2 <- data.table::rbindlist(tidal_list)

tidal2$V1 <- dmy(tidal2$V1)
tidal2$year <- year(tidal2$V1)

tidal2$source_file <- str_replace(tidal2$source_file, "./data/external/tidal_2018/eel_", "") 
tidal2$source_file <- str_replace(tidal2$source_file, "_new.dat", "")


# Bind datasets together
tidal2 <- rbind(tidal, tidal2)



# Load rest of the tidal data
tidal_list_names <- list.files(path = "./data/external/tidal_data_lianne_harrison/",
                    pattern = "*.dat", 
                    full.names = T)

tidal_list <- lapply(tidal_list_names, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

tidal <- data.table::rbindlist(tidal_list)

tidal$V1 <- dmy(tidal$V1)

# Merge both tidal datasets
tidal2$year <- NULL
tidal <- rbind(tidal, tidal2)




# Format columns
tidal$ID <- str_replace(tidal$source, "./data/external/tidal_data_lianne_harrison/eel_", "") 
tidal$ID <- str_replace(tidal$ID, ".dat", "") 
tidal$ID <- factor(tidal$ID)
tidal$source_file <- NULL

tidal <- tidal %>%
  rename(date = V1,
         time = V2,
         current_lon = V3,
         current_lat = V4,
         U = V5,
         V = V6,
         speed = V7,
         direction = V8)

tidal$datetime <- paste(tidal$date, tidal$time)
tidal$datetime <- ymd_hms(tidal$datetime)



# Adjust ID names
tidal$ID <- recode_factor(tidal$ID, 
                           "157002" = "15700_2",
                           "157302" = "15730_2",
                           "174922" = "17492",
                           "174922021" = "17492_2",
                           "174992" = "17499_2",
                           "175132" = "17513_2",
                           "175182" = "17518_2",
                           "175252" = "17525_2",
                           "176522" = "17652_2")

# Set tidal dataset to dataframe
tidal <- as.data.frame(tidal)

# Select relevant eels
tidal <- filter(tidal, ID == "15805" |
                    ID == "15730" |
                    ID == "15757" |
                    ID == "15700" |
                    ID == "15714" |
                    ID == "16031" |
                    ID == "15706" |
                    ID == "15981" |
                    ID == "15777" |
                    ID == "17443" |
                    ID == "17499" |
                    ID == "17513" |
                    ID == "17534" |
                    ID == "17526" |
                    ID == "17522" |
                    ID == "17492" |
                    ID == "17508" |
                    ID == "17536" |
                    ID == "17538" |
                    ID == "17537" |
                    ID == "17510" |
                    ID == "15789" |
                    ID == "17521" |
                    ID == "17535" |
                    ID == "17653" |
                    ID == "15730_2" |
                    ID == "15700_2" |
                    ID == "17646" |
                    ID == "17642" |
                    ID == "17658" |
                    ID == "17525_2" |
                    ID == "17492_2" |
                    ID == "17518_2" |
                    ID == "17638" |
                    ID == "17634" |
                    ID == "17547" |
                    ID == "17635" |
                    ID == "17487" |
                    ID == "17499_2" |
                    ID == "17663" |
                    ID == "17513_2" |
                    ID == "17648" )

tidal$ID <- factor(tidal$ID) 

# 3. Link tidal data to dataset ####
# When running the code on the complete dataset, R runs stuck. Hence, run the code on subsets and bind them together
unique(data$ID)
data_ind <- filter(data, ID == "17499_2")
tidal_ind <- filter(tidal, ID == "17499_2")

data17499_2 <- data_ind %>%
  rowwise() %>%
  dplyr::mutate(diffs_tidal = list(abs(datetime - tidal_ind$datetime))) %>%
  dplyr::mutate(tidal_datetime = tidal_ind$datetime[which(diffs_tidal == min(diffs_tidal, na.rm = TRUE))][1])

data <- do.call("rbind", list(data15805,
                               data15730,
                               data15757,
                               data15700,
                               data15714,
                               data15706,
                               data16031,
                               data15777,
                               data15981,
                               data17443,
                               data17499,
                               data17492,
                               data17510,
                               data17526,
                               data17534,
                               data17508,
                               data17538,
                               data17521,
                               data17536,
                               data17522,
                               data17513,
                               data17535,
                               data15730_2,
                               data15700_2,
                               data15789,
                               data17537,
                               data17658,
                               data17653,
                               data17646,
                               data17648,
                               data17663,
                               data17642,
                               data17634,
                               data17525_2,
                               data17635,
                               data17638,
                               data17547,
                               data17492_2,
                               data17487,
                               data17518_2,
                               data17513_2,
                               data17499_2))


# Set hourly resolution in tracking dataset
#data$datehour <- lubridate::floor_date(data$datetime, "hour")
#data$datehour <- format(round(data$datetime, units="hours"), format="%Y-%m-%d %H:%M:%S")
#data$datehour <- ymd_hms(data$datehour)

# Set hourly resolution in tidal dataset
#tidal$datehour <- lubridate::floor_date(tidal$datetime, "hour")
#tidal$datehour <- format(round(tidal$datetime, units="hours"), format="%Y-%m-%d %H:%M:%S")
#tidal$datehour <- ymd_hms(tidal$datehour)

# Remove double dates per eel (ID) from tidal dataset
tidal <- rename(tidal, tidal_datetime = datetime)
tidal <- tidal[!duplicated(tidal[c('ID','tidal_datetime')]),] 
tidal <- tidal %>%     # Add ID number to duplicate dates
  group_by(ID, tidal_datetime) %>%
  add_tally()

duplicates <- filter(tidal, n == 2)   # Filter duplicate dates
duplicates <- duplicates %>%             # Add ID number to distinguish between first and second duplicate
  mutate(number_id = row_number())
duplicates <- filter(duplicates, number_id == 2)  # Filter second duplicates

tidal <- filter(tidal, n != 2)   # Remove duplicate dates from tidal dataset

# Bind 'duplicates' dataset with second duplicates to the tidal dataset
tidal <- ungroup(tidal)
tidal$n <- NULL
duplicates <- ungroup(duplicates)
duplicates$n <- NULL
duplicates$number_id <- NULL

tidal <- rbind(tidal, duplicates)


# Merge tracking dataset with tidal dataset
data_tidal <- left_join(data, tidal, by = c('ID', 'tidal_datetime'))

# Process dataset
data_tidal$date <- NULL
data_tidal$time <- NULL
data_tidal$diffs_tidal <- NULL

# 4. write csv ####
write.csv(data_tidal, "./data/interim/data_circadian_tidal_5min.csv")
