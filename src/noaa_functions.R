#' Functions to support the download environmental data from NOAA station and
#' link to tracking data
#' 
#'  Lifewatch 2021
#'  
#'  Damiano Oldoni


#' Wrapper function around worldmet function importNOAA() to import
#' meteorological data
#'
#' This function downloads and select specific data from a NOAA station.
#'
#' @param station_code (character) a station code ,e.g. "031405-99999"
#' @param station_nickname (character) a short nickname for the station, e.g.
#'   "andrew"
#' @param hourly_date (logical) Should hourly means be calculated?
#' @param years (numeric) The year to import. This can be a vector of years e.g.
#'   year = 2000:2005
#' @param path (character) Path to folder containing NOAA data files 
#'   
get_data_noaa <- function(station_code, 
                          station_nickname, 
                          hourly_data, 
                          years,
                          path) {
  
  filename = glue("{ station_code }_{ year_start }-{ year_end }_{ nickname }.csv",
                  station_code = station_code,
                  year_start = years[1],
                  year_end = years[2],
                  nickname = station_nickname)
  is.dir(path)
  if (filename %in% list.files(path)) {
    message(glue("Reading data of station '{ nickname }' ({ code }) from file { path }/{ filename }",
                 nickname = station_nickname,
                 code = station_code,
                 path = path,
                 filename = filename))
    noaa_data <- read_csv(file = glue("{path}/{filename}",
                                      path = path,
                                      filename = filename),
                          na = "",
                          col_types = cols(code = col_character(),
                                           station = col_character(),
                                           date = col_datetime(""),
                                           latitude = col_number(),
                                           longitude = col_number(),
                                           cl = col_number()))
  } else {
    message(glue("Importing data of station '{ nickname }' ({ code })",
                 nickname = station_nickname,
                 code = station_code))
    noaa_data <- importNOAA(code = station_code, hourly = hourly_data, year = years)
  
    if (!"cl" %in% names(noaa_data)) {
      # haumet station (070220-99999) has no cl
      warning(glue("Station {station} has no column 'cl'. Created by duplicating values of 'cl_1'.", station = station_code))
      noaa_data$cl <- noaa_data$cl_1
    }
    
    noaa_data <- 
      noaa_data %>% 
      # select relevant columns
      select(code, station, date, latitude, longitude, cl) %>%
      # set date as datetime object
      mutate(date = as_datetime(date)) %>%
      mutate(code = as.character(code))
    
    message(glue("Saving data in file {path}/{filename}",
                 path = path,
                 filename = filename))
    save_data_noaa_csv(noaa_df = noaa_data, 
                       filename = filename,
                       path = path)
    return(noaa_data)
  }
}


#' Save NOAA stations data 
#'
#' Function to save data retrieved via `get_data_noaa()` as csv file
#' 
#' @param noaa_df (data.frame) A df containing the NOAA environmental data to save
#' @param filename (character) Name of the csv file which will be saved
#' @param path (character) Directory where csv file will be saved
save_data_noaa_csv <- function(noaa_df, 
                               filename,
                               path) {
  is.dir(path)
  is.character(filename)
  file <- glue("{ path }/{ filename }",
               path = path,
               filename = filename)
  write_csv(x = noaa_df,
            file = file,
            na = "")
  message(glue("Saving { filename } completed. Size: { size_file } bytes",
               path = path,
               filename = filename,
               size_file = file.size(file))
  )
}

#' Function to retrieve the nearest NOAA env stations
#'
#' @param rowID (numeric) row number to read from (see  `distance_df` below)
#' @param dist_threshold distance threshold in meters (10km = 10^4 meters)
#' @param distance_df (data.frame) a data.frame containing the distance between
#'   DST tracking data (rows) and NOAA environmental stations (columns)
get_nearest_stations <- function(rowID,
                                 dist_threshold,
                                 distance_df,
                                 tracking_data) {
  dist_df_sorted <- as.list(sort(distance_df[rowID,]))
  dist_df_near_stations <- dist_df_sorted[dist_df_sorted <= dist_threshold]
  track_lat <- tracking_data[rowID,]$lat
  track_lon <- tracking_data[rowID,]$lon
  if (length(dist_df_near_stations) == 0) {
    message(glue("Row { rowID } No stations found in the neighborhood ({ threshold } m) of ({ lat },{ lon }). Nearest station: { station_code } ({ distance })",
                 rowID = rowID,
                 threshold = dist_threshold,
                 lat = track_lat,
                 lon = track_lon,
                 station_code = names(dist_df_sorted[1]),
                 distance = dist_df_sorted[1]))
    NA
  } else {
    dist_df_near_stations
  }
}

#' Function to retrieve the best fitting NOAA env station data
#'
#' The fitting is based on a geographical and temporal threshold.
#' @param datetime_track (datetime) tracking date and time
#' @param ordered_noaa_stations (list) list containing the distances of the
#'   nearest stations as returned from function `get_nearest_stations`
#' @param time_threshold_hours (numeric) temporal threshold in hours
#' @param rowID (numberic) unique identifier of tracking data which will be
#'   added to output to join environmental data with tracking data
#' @param df_noaa_stations (data.frame) environmental data from NOAA
#'   environmental stations
get_best_env_data <- function(datetime_track,
                              ordered_noaa_stations,
                              timethreshold_hours,
                              rowID,
                              df_noaa_stations){
  if (!is.na(ordered_noaa_stations)) {
    env_data <- 
      df_noaa_stations %>%
      # filter by "space"
      filter(code %in% names(ordered_noaa_stations)) %>%
      # filter by "time"
      mutate(abs_diff_datetime = abs(date - datetime_track)) %>%
      filter(abs_diff_datetime < hours(timethreshold_hours))
    
    noaa_stations_left <- 
      env_data %>%
      distinct(code) %>%
      pull()
    
    ordered_noaa_stations_left <- 
      ordered_noaa_stations[names(ordered_noaa_stations) %in% noaa_stations_left]
    
    # final filter by taking the nearest station and the nearest time
    if (nrow(env_data) > 0) {
      env_data %>%
        # get data from nearest station left
        filter(code == names(ordered_noaa_stations_left)[1]) %>%
        # get the temporal nearest data
        filter(abs_diff_datetime == min(abs_diff_datetime)) %>%
        # if datetime difference is exactly the same (e.g. 18:30:00 is 30 mins
        # from 19:00:00 and 18:00:00 as well) take the first row (typically the
        # earlier datetime)
        head(1) %>%
        mutate(row_id = rowID)
    } else {
      message(glue("No data from NOAA stations in the neighborhood for datetime { dt }",
                   dt = datetime_track))
    }
  }
}
