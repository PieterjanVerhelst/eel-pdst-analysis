

### Concept of the raw data file:
#
# Header info -> variable amount of lines
#   * TAG ID
#   * Firmware version and build level
#   * Lifetime notebook contents: ID, pressure range and number of sensors
#   * Deployment notebook contents
#   * Mission Parameters
#   * Daylog contents
# 
# Effective daylog table data
#   * Mission Day,Date,Max Temp,Min Temp,Max Depth,Min Depth,Batt Volts
# 
# Data Block 1 - Pressure
#   * Header info: start time, stop time, logging rate, resolution, Data points available
#   * Time Stamp,Pressure
# 
# Data Block 2 - Temperature
#   * Header info: start time, stop time, logging rate, resolution, Data points available
#   * Time Stamp,Temp
# 
# End line "No Fast Data"


library(iterators)
library(stringr)
library(readr)
library(tidyr)

filename <- "data/A15716_15-01-2019.csv"

################################################
# Reading metadata and info
################################################

pdst_get_data_blocks_info <- function(filename) {

  pdst_con <- file(filename, "r")
  header_it <- ireadLines(pdst_con)
  line <- nextElem(header_it)
  track_tag_id <- str_extract(line, "(?<=Tag )[A-Z|0-9]*")
  
  # metadata extraction (can be extended) ------ 
  cnt <- 1
  while (!startsWith(line, "Daylog data for last deployment")) {
    # extract metadata
    if (startsWith(line, "No of sensors")) {
      track_sensors_no <- as.integer(str_split(line, pattern = ",", 
                                               simplify = TRUE)[2])
    }
    if (startsWith(line, "Pressure Range")) {
      track_pressure_range <- str_split(line, pattern = ",", simplify = TRUE)[2]
    }
    if (startsWith(line, "Total Days Alive")) {
      track_total_days <- as.integer(str_split(line, pattern = "=", 
                                               simplify = TRUE)[2])
    }  
    line <- nextElem(header_it)
    cnt <- cnt + 1
  }
  print(cnt)
  
  # check for start and length of the Daylog data
  daylog_header_line <- nextElem(header_it)
  cnt <- cnt + 1
  daylog_skip <- cnt # startline for daylog
  # after header line, counting till next empty line to get daylog length
  # (track_total_days in metadata is not reliable!)
  daylog_length <- 0
  while (line != "") {  # Data Block 1
    daylog_length <- daylog_length + 1
    cnt <- cnt + 1
    line <- nextElem(header_it)
  }
  daylog_length <- daylog_length - 1
  
  # prepare col names of daylog data block
  col_names_original <- str_to_lower(str_split(daylog_header_line, pattern = ",", 
                                               simplify = TRUE))
  col_names <- str_replace_all(col_names_original, " ", "_")
  for (i in seq(3, 11, 2)) {
    col_names <- append(col_names, paste(col_names[i], "decimal", sep = "_"), 
                        after = i)  
  }
  daylog_col_names <- col_names
  
  daylog <- list('daylog_skip' = daylog_skip, 
                 'daylog_length' = daylog_length,
                 'daylog_col_names' = daylog_col_names)
  
  # check for start and length of the Data blocks
  # 1. pressure
  while (!startsWith(line, "Data points available")) {
    line <- nextElem(header_it)
    cnt <- cnt + 1
  }
  pressure_length <- as.integer(str_split(line, pattern = "=", simplify = TRUE)[2])
  pressure_header_line <- nextElem(header_it)
  pressure_header <- str_to_lower(str_split(pressure_header_line, 
                                            pattern = ",", simplify = TRUE))
  pressure_col_names <- append(pressure_header, "decimal")
  cnt <- cnt + 1
  pressure_skip <- cnt # startline for pressure data block
  
  pressure <- list('pressure_skip' = pressure_skip, 
                   'pressure_length' = pressure_length,
                   'pressure_col_names' = pressure_col_names)
  
  # 2. temperature
  line <- nextElem(header_it) # move beyond Data Block 1
  cnt <- cnt + 1
  while (!startsWith(line, "Data points available")) {
    line <- nextElem(header_it)
    cnt <- cnt + 1
  }
  temp_length <- as.integer(str_split(line, pattern = "=", simplify = TRUE)[2])
  temp_header_line <- nextElem(header_it)
  temp_header <- str_to_lower(str_split(temp_header_line, 
                                        pattern = ",", simplify = TRUE))
  temp_col_names <- append(temp_header, "decimal")
  cnt <- cnt + 1
  temp_skip <- cnt # startline for temperature data block
  
  temperature <- list('temp_skip' = temp_skip, 
                      'temp_length' = temp_length,
                      'temp_col_names' = temp_col_names)
  
  close(pdst_con)
  
  return(list('daylog' = daylog, 
              'pressure' = pressure, 
              'temperature' = temperature))
}

pdst_get_data_blocks_info(filename)

################################################
# Reading the data blocks
################################################

# daylog_skip ; daylog_length

# Read the daylog data.frame
if (track_total_days != 0) {  # ! Total Days Alive can be zero as well
  col_types <- cols(
    .default = col_character(),
    mission_day = col_integer(),
    date = col_date("%d/%m/%Y")
  )
  track_day_log_data <- read_csv(filename, skip = daylog_skip, col_names = col_names, 
                                 n_max = daylog_length, col_types = col_types) # 
  print(head(track_day_log_data))
}

# combine columns and decimal counterpart to number column
decimal_values <- track_day_log_data %>% 
  select(matches("decimal")) %>%
  gather(key = "variable_name", value = "decimal_value") %>%
  select(decimal_value)
track_daylog_df <- track_day_log_data %>% 
  select(-matches("decimal"), -mission_day, -date) %>%
  tibble::rowid_to_column() %>%
  gather(key = "variable_name", value = "value", -rowid) %>%
  bind_cols(decimal_values) %>%
  unite(value, value, decimal_value, sep = '.') %>%
  mutate(value = parse_number(value)) %>%
  spread(key = variable_name, value = value) %>%
  select(-rowid) %>%
  bind_cols(track_day_log_data %>% select(mission_day, date))
 
# For pressure, read Data Block ----
pressure_data_sensor <- read_csv(filename, skip = pressure_skip, 
                              col_names = pressure_col_names,
                              n_max = pressure_length, col_types = cols())
pressure_data_sensor <- pressure_data_sensor %>%
  mutate(pressure_value = parse_number(paste(.$pressure, .$decimal, sep = "."))) %>%
  rename(datetime = `time stamp`) %>% select(datetime, pressure_value)

# For temperature, read Data Block ----
temp_data_sensor <- read_csv(filename, skip = temp_skip, 
                             col_names = temp_col_names,
                             n_max = temp_length, col_types = cols())
temp_data_sensor <- temp_data_sensor %>%
  mutate(temp_value = parse_number(paste(.$temp, .$decimal, sep = "."))) %>%
  rename(datetime = `time stamp`) %>% select(datetime, temp_value)






