#
# pdst sensor read functionalities
# 
# Van Hoey S. 2019
# Lifewatch INBO
# 

### Concept/structure of the raw data file  -------
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

#' Read metadata and info from pdst data file
#' 
#' Notice: this funnction works with an iterator looping the entire file, which
#' is not at all efficient. However, as we do not have a more specific format 
#' definition and the length of the daylog section is not properly defined,
#  this will extract the required info to load the data sections
#'
#' @param filename char Filename of a pdst raw data file
#'
#' @return list of lists Daylog, pressure and temperature info, which for each
#' of the lists 3 elements: xx_skip, i.e. startline of data section;
#' xx_length, i.e.number of data lines and xx_col_names, i.e. names of the 
#' columns, taking into account additional decimal column(s).
#'
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
      print(paste("The data file contains", 
                  track_sensors_no, "sensor data series." ))
      if (track_sensors_no != 2) {
        stop("Function only supports 2 sensors in data file: ",
             "pressure and temperature")
      }
    }
    line <- nextElem(header_it)
    cnt <- cnt + 1
  }
  
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

#' Read daylog data section
#'
#' @param filename char Filename of a pdst raw data file
#' @param daylog_skip int
#' @param daylog_length int
#' @param daylog_col_names char
#'
#' @return
#' @export
#'
#' @examples
pdst_read_daylog <- function(filename, daylog_skip, daylog_length, 
                             daylog_col_names) {

  if (track_total_days == 0) {  # ! Total Days Alive can be zero as well
    stop("File ", filename, " contains no daylog information!")
  }

  col_types <- cols(
    .default = col_character(),
    mission_day = col_integer(),
    date = col_date("%d/%m/%Y")
  )
  track_day_log_data <- read_csv(filename, skip = daylog_skip, 
                                 col_names = daylog_col_names, 
                                 n_max = daylog_length, 
                                 col_types = col_types) # 
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
 
  return(track_day_log_data) 
}

#
# Generic for sensor data, read Data Block ----
#
pdst_read_sensor <- function(filename, line_skip, line_length, 
                                  variable = "temperature") {
  col_names <- c("datetime", "variable", "decimal")
  data_sensor <- read_csv(filename, skip = line_skip, 
                          col_names = col_names, 
                          n_max = line_length, col_types = cols())
  data_sensor <- data_sensor %>%
    mutate(value = parse_number(paste(.$variable, 
                                      .$decimal, sep = "."))) %>%
    select(datetime, value)
  colnames(data_sensor) <- c("datetime", variable)
  return(data_sensor)
}


# Apply functions on case

filename <- "data/A15700_10-01-2019.csv"
data_info <- pdst_get_data_blocks_info(filename)

pressure_data <- pdst_read_sensor(filename, 
                   data_info$pressure$pressure_skip,
                   data_info$pressure$pressure_length,
                   "pressure")
temp_data <- pdst_read_sensor(filename, 
                    data_info$temperature$temp_skip,
                    data_info$temperature$temp_length,
                    "temperature")







