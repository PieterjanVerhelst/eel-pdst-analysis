# Classify current phases
# By Raïsa Carmen
# raisa.carmen@inbo.be


# Load packages
library(tidyverse) 
library(lubridate)


# 1. Read data ####
data <- read.csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data <- data %>%
  rename(direction_x = U,
         direction_y = V)

# Arrange data set according to ID and datetime
data <- data %>% 
  arrange(ID, datetime)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]


# Classify currents
# current in x-direction
data$counter <- NA
counter <- 1
for(i in 2:nrow(data)){
  if((is.na(data[i-1,"direction_x"]) | is.na(data[i,"direction_x"]) |
      (data[i,"direction_x"] == data[i-1,"direction_x"])) &
     data[i, "ID"] == data[i - 1, "ID"]){
    data[i, "counter"] <- counter
  }
  else{counter <- counter+1
  data[i, "counter"] <- counter
  }
}

previous <- data %>% group_by(ID, counter) %>%
  summarize(direction_x_prev = first(direction_x),
            previous_ID = first(ID)) %>%
  mutate(counter = lead(counter)) %>%
  ungroup()

data <- data %>% left_join(previous) %>%
  mutate(current_phase_x = ifelse(ID == previous_ID,
                              ifelse(direction_x >= direction_x_prev,
                                     "eastward",
                                     "westward"),
                              NA)
  ) %>%
  dplyr::select(-counter, -previous_ID, -direction_x_prev)


# current in y-direction
data$counter <- NA
counter <- 1
for(i in 2:nrow(data)){
  if((is.na(data[i-1,"direction_y"]) | is.na(data[i,"direction_y"]) |
      (data[i,"direction_y"] == data[i-1,"direction_y"])) &
     data[i, "ID"] == data[i - 1, "ID"]){
    data[i, "counter"] <- counter
  }
  else{counter <- counter+1
  data[i, "counter"] <- counter
  }
}

previous <- data %>% group_by(ID, counter) %>%
  summarize(direction_y_prev = first(direction_y),
            previous_ID = first(ID)) %>%
  mutate(counter = lead(counter)) %>%
  ungroup()

data <- data %>% left_join(previous) %>%
  mutate(current_phase_y = ifelse(ID == previous_ID,
                                  ifelse(direction_y >= direction_y_prev,
                                         "northward",
                                         "southward"),
                                  NA)
  ) %>%
  dplyr::select(-counter, -previous_ID, -direction_y_prev)



# write csv
write.csv(data, "./data/interim/data_current_phases.csv")

