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
data <- read_csv("./data/interim/data_circadian.csv")
data$X1 <- NULL
data$X1_1 <- NULL
data$ID <- factor(data$ID)


# 2. Load tidal data ####
tidal_list_names <- list.files(path = "./data/external/tidal_data_john_aldridge/",
                    pattern = "*.dat", 
                    full.names = T)

tidal_list <- lapply(tidal_list_names, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

tidal <- data.table::rbindlist(tidal_list)

# Format columns
tidal$ID <- str_replace(tidal$source, "./data/external/tidal_data_john_aldridge/eel_", "") 
tidal$ID <- str_replace(tidal$ID, ".dat", "") 
tidal$ID <- factor(tidal$ID)
tidal$source_file <- NULL

tidal <- tidal %>%
  rename(date = V1,
         time = V2,
         lon = V3,
         lat = V4,
         U = V5,
         V = V6,
         speed = V7,
         direction = V8)

tidal$datetime <- paste(tidal$date, tidal$time)
tidal$datetime <- dmy_hms(tidal$datetime)

# set tidal dataset to dataframe
tidal <- as.data.frame(tidal)


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
write.csv(data_tidal, "./data/interim/data_circadian_tidal.csv")
