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
tidal_list_names <- list.files(path = "./data/external/tidal_data_lianne_harrison/",
                    pattern = "*.dat", 
                    full.names = T)

tidal_list <- lapply(tidal_list_names, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

tidal <- data.table::rbindlist(tidal_list)

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
tidal$datetime <- dmy_hms(tidal$datetime)

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

# set tidal dataset to dataframe
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
# Set hourly resolution
data$datehour <- lubridate::floor_date(data$datetime, "hour")
tidal$datehour <- lubridate::floor_date(tidal$datetime, "hour")

# Remove double dates per eel (ID) from tidal dataset
#tidal <- tidal[!duplicated(tidal[c('ID','datehour')]),] 
tidal <- tidal %>%     # Add ID number to duplicate dates
  group_by(ID, datehour) %>%
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
data_tidal <- left_join(data, tidal, by = c('ID', 'datehour'))

# Process dataset
data_tidal <- rename(data_tidal, datetime = datetime.x)
data_tidal$datetime.y <- NULL
data_tidal$date <- NULL
data_tidal$time <- NULL


# 4. write csv ####
write.csv(data_tidal, "./data/interim/data_circadian_tidal_5min.csv")
