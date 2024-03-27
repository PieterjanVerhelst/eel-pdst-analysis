#
# pdst sensor read functionalities
# 
# Van Hoey S. 2019
# Oldoni D. 2024
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

library(stringr)
library(readr)
library(tidyr)
library(dplyr)
library(rlang)

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
#' of the lists 3 elements: 
#' the first two elements are the same for daylog and sensor:
#'     * xx_skip, i.e. startline of data section;
#'     * xx_length, i.e.number of data lines 
#' The last on is:
#'     * xx_col_names for daylog, i.e. names of the columns, taking into account 
#'       additional decimal column(s).
#'     * xx_name for the sensor data, i.e. name of the variable
#'     
pdst_get_data_blocks_info <- function(filename, version) {
  
  pdst_con <- file(filename, "r")
  line <- readLines(pdst_con, n = 1)
  
  # metadata extraction (can be extended) ------ 
  cnt <- 1
  
  while (!str_starts(
    string = line,
    pattern = "Tag ID"
      )
    ) {
    
    # extract metadata
    if (str_starts(line, "Firmware Version No,")) {
      firm_version <- str_extract(line, "(?<=Firmware Version No,).*")
    }
    if (str_starts(line, "Firmware Build Level,")) {
      build_level <- str_extract(line, "(?<=Firmware Build Level,).*")
    }
    line <- readLines(pdst_con, n = 1)
    cnt <- cnt + 1
    
  }
  
  # define firmware version/build level
  firmware_version <- str_c(firm_version, build_level, sep = ".")
  if (firmware_version == "3.80") {
    p <- "Daylog data for last deployment"
  }
  if (firmware_version == "2.70") {
    p <- "The following data are the Daylog contents"
  }
  
  while (!str_starts(
    string = line,
    pattern = p
    )
  ) {
    if (str_starts(line, "Tag ID")) {
      track_tag_id <- str_extract(line, "(?<=Tag ID,)[A-Z|0-9]*")
    }
    
    if (str_starts(line, "No of sensors ,")) {
      track_sensors_no <- as.integer(
        str_extract(string = line,
                    pattern = "(?<=No of sensors ,).*"
        )
      )
      print(paste("The data file", filename, "contains", 
                  track_sensors_no, "sensor data series."))
      if (track_sensors_no != 2) {
        stop("Function only supports 2 sensors in data file.")
      }
    }
    if (str_starts(line, "Total Days Alive")) {
      track_total_days <- as.integer(str_split(line, pattern = "=", 
                                               simplify = TRUE)[2])
      if (track_total_days == 0) {  # ! Total Days Alive can be zero as well
        warning("File ", filename, " contains no daylog information!")
      }
    }
    line <- readLines(pdst_con, n = 1)
    cnt <- cnt + 1
  }
  
  
  # check for start and length of the Daylog data
  daylog_header_line <- readLines(pdst_con, n = 1)
  cnt <- cnt + 1
  daylog_skip <- cnt # startline for daylog
  # after header line, counting till next empty line to get daylog length
  # (track_total_days in metadata is not reliable!)
  daylog_length <- 0
  while (line != "") {  # Data Block 1
    daylog_length <- daylog_length + 1
    cnt <- cnt + 1
    line <- readLines(pdst_con, n = 1)
  }
  daylog_length <- daylog_length - 1
  
  # prepare col names of daylog data block
  
  if (daylog_length == 0) {
    daylog_col_names <- c()
  } else {
    col_names_original <- str_to_lower(str_split(daylog_header_line, pattern = ",", 
                                                 simplify = TRUE))
    col_names <- str_replace_all(col_names_original, " ", "_")
    daylog_col_names <- col_names
  }
  
  daylog <- list('daylog_skip' = daylog_skip, 
                 'daylog_length' = daylog_length,
                 'daylog_col_names' = daylog_col_names)
  
  # check for start and length of the Data blocks
  # 1. pressure
  while (!str_starts(line, "Data points available")) {
    line <- readLines(pdst_con, n = 1)
    cnt <- cnt + 1
  }
  pressure_length <- as.integer(str_split(line, pattern = "=", simplify = TRUE)[2])
  pressure_header_line <- readLines(pdst_con, n = 1)
  pressure_header <- str_to_lower(str_split(pressure_header_line, 
                                            pattern = ",", simplify = TRUE))
  pressure_name <- pressure_header[2]
  cnt <- cnt + 1
  pressure_skip <- cnt # startline for pressure data block
  
  pressure <- list('pressure_skip' = pressure_skip, 
                   'pressure_length' = pressure_length,
                   'pressure_name' = pressure_name)
  
  # 2. temperature
  line <- readLines(pdst_con, n = 1) # move beyond Data Block 1
  cnt <- cnt + 1
  while (!str_starts(line, "Data points available")) {
    line <- readLines(pdst_con, n = 1)
    cnt <- cnt + 1
  }
  temp_length <- as.integer(str_split(line, pattern = "=", simplify = TRUE)[2])
  temp_header_line <- readLines(pdst_con, n = 1)
  temp_header <- str_to_lower(str_split(temp_header_line, 
                                        pattern = ",", simplify = TRUE))
  temp_name <- temp_header[2]
  cnt <- cnt + 1
  temp_skip <- cnt # startline for temperature data block
  
  temperature <- list('temp_skip' = temp_skip, 
                      'temp_length' = temp_length,
                      'temp_name' = temp_name)
  
  close(pdst_con)
  
  return(list('track_tag_id' = track_tag_id,
              'daylog' = daylog, 
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
#' @return data.frame | tibble
#'
#' @examples
pdst_read_daylog <- function(filename, daylog_skip, daylog_length, 
                             daylog_col_names) {
  
  col_types <- cols(
    .default = col_double(),
    mission_day = col_integer(),
    date = col_date("%d/%m/%Y")
  )
  track_day_log_data <- read_csv(filename, skip = daylog_skip, 
                                 col_names = daylog_col_names, 
                                 n_max = daylog_length, 
                                 col_types = col_types)
  return(track_day_log_data) 
}

#' Read single sensor data section
#'
#' @param filename char Filename of a pdst raw data file
#' @param line_skip int first line of the dataset
#' @param line_length int number of lines of the data set
#' @param variable char variable i.e. 'temperature' or 'pressure'
#'
#' @return data.frame | tibble
#'
#' @examples
pdst_read_sensor <- function(filename, line_skip, line_length, 
                             variable = "temperature") {
  col_names <- c("datetime", {{variable}})
  data_sensor <- read_csv(filename, skip = line_skip, 
                          col_names = col_names, 
                          n_max = line_length, col_types = cols())
  return(data_sensor)
}


#' Read pdst sensor data from raw file
#' 
#' Wraps the other functions, first scanning the file on different sections,
#' next load and combine the sensor data
#' 
#' @param filename char Filename of a pdst raw data file
#' 
#' 
pdst_read_file <- function(filename) {
  data_info <- pdst_get_data_blocks_info(filename)
  
  # read daylog data
  if (data_info$daylog$daylog_length == 0) {
    daylog_data <- NA
  } else {
    daylog_data <- pdst_read_daylog(filename,
                                    data_info$daylog$daylog_skip, 
                                    data_info$daylog$daylog_length,
                                    data_info$daylog$daylog_col_names) %>%
      mutate(track_tag_id = data_info$track_tag_id)    
  }
  
  # read sensor data
  message("Read sensor data: pressure.")
  pressure_data <- pdst_read_sensor(filename, 
                                    data_info$pressure$pressure_skip,
                                    data_info$pressure$pressure_length,
                                    "pressure")
  message("Read sensor data: temperature.")
  temp_data <- pdst_read_sensor(filename, 
                                data_info$temperature$temp_skip,
                                data_info$temperature$temp_length,
                                "temperature")
  # combine sensor data
  message("Combine sensor data.")
  sensor_data <- full_join(pressure_data, temp_data, by = "datetime") %>%
    mutate(track_tag_id = data_info$track_tag_id)
  
  return(list("track_tag_id" = data_info$track_tag_id,
              "daylog" = daylog_data, 
              "sensor" = sensor_data))
}
