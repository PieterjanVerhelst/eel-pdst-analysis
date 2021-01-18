# Calculate number of vertical migrations per hour
# By Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be

# Packages
library(tidyverse) # To do datascience
library(tidylog)  # To get infos about dplyr functions
library(lubridate)



# 1. Import data ####
data <- read_csv("./data/interim/data_depth_diff.csv",
                 na = "", 
                 col_types = list(ID = col_factor(),
                                  night_day = col_factor(),
                                  datetime = col_datetime(),
                                  datehour = col_datetime(),
                                  depth_change = col_double(),
                                  speed = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$X1 <- NULL
data$X1_1 <- NULL
data$X <- NULL


# 2. Remove lines with depth change <= 0.30 m (resolution error)
data <- filter(data, depth_change > 0.30)
summary(data$depth_change)


# 3. Calculate number of vertical migrations per hour per eel with hourly summaries of environmental data
hourly <- data %>%
  group_by(ID, datehour) %>%
  summarise(n = n(),
            mean_temp = mean(temperature),
            mean_moon_fraction = mean(moon_fraction),
            speed = mean(speed, na.rm = TRUE),         # speed and direction are hourly measuresments, so mean keeps same value
            direction = mean(direction, na.rm = TRUE))
summary(hourly)


# 4. Select circadian phases per hour
circadian <- data %>%
  group_by(ID, datehour) %>%
  select(ID, datehour, night_day) %>%
  distinct()


# 5. Remove second duplicate in circadian phase
# During a 'twilight hour', day can change to night and vice versa
# Here I decided to keep the first circadian classification
# e.g. if an hour started as 'night', it will be flagged as 'night'
circadian <- circadian[!duplicated(circadian[c('ID','datehour')]),] 


# 6. Merge hourly vertical migrations with environmental summaries to circadian phases
hourly <- left_join(hourly, circadian, by = c("ID", "datehour"))


# 7. Write csv ####
write.csv(hourly, "./data/interim/data_hourly_migrations.csv")






