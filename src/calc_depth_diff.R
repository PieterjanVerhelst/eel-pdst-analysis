# Calculate depth differences between minima and maxima
# By Damiano Oldoni
# damiano.oldoni@inbo.be

# Packages
library(tidyverse) # To do datascience
library(tidylog)  # To get infos about dplyr functions
library(lubridate)



# 1. Import data ####
data <- read_csv("./data/interim/data_current_phases.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  U = col_double(),
                                  V = col_double(),
                                  speed = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]


# Arrange data set according to tag ID and datetime, so min and max are calculated accordingly
data <-
  data %>%
  arrange(ID, datetime)


# 2. Find minima and maxima ####
data <-
  data %>%
  group_by(ID) %>%
  mutate(is_maximum = if_else(corrected_depth - lag(corrected_depth, 1) > 0 &
                                corrected_depth - lead(corrected_depth, 1) >= 0,
                              TRUE,
                              FALSE),
         is_minimum = if_else(corrected_depth - lag(corrected_depth, 1) < 0 &
                                corrected_depth - lead(corrected_depth, 1) <= 0,
                              TRUE,
                              FALSE)
  ) %>%
  ungroup()

# 3. Take minima and maxima only ####
data_min_max <-
  data %>%
  filter(is_maximum == TRUE | is_minimum == TRUE)

# 4. Calculate changes in max and min depths ####
data_min_max <-
  data_min_max %>%
  group_by(ID) %>%
  mutate(depth_change = abs(corrected_depth - lag(corrected_depth)),
         diving_time = numericdate - lag(numericdate)) %>%
  ungroup()

data_min_max$diving_speed <- data_min_max$depth_change/data_min_max$diving_time


# Remove depth changes below 1 meter
#data_min_max_1m <-
#  data_min_max %>%
#  filter(depth_change >= 1)

ggplot(data_min_max) +
  geom_histogram(aes(depth_change, color = night_day),
                 binwidth = 10,
                 position = "dodge",
                 fill = "white") +
  facet_wrap(~ID, nrow = 2)

# Arrange depth changes in descending order and see in which phase of the day if occurs
data_min_max %>%
  arrange(desc(depth_change)) %>%
  dplyr::select(ID, night_day, depth_change)


# Calculate summary values
data_min_max_no_na <- data_min_max[!is.na(data_min_max$depth_change),]
aggregate(data_min_max_no_na$depth_change, list(data_min_max_no_na$night_day, data_min_max_no_na$ID), mean)


# 5. Summarise data per hour ####
# --> calculate vertical movement per hour (total movement, not range!)
#data$date_hour <- floor(data$datetime/3600)
data_min_max$date_hour <- lubridate::floor_date(data_min_max$datetime, "hour")  


# Calculate hourly depth change
hourly_depth_change <- data_min_max %>%
  select(ID, date_hour, depth_change) %>%
  group_by(ID, date_hour) %>%
  summarise(hourly_depth_change = max(depth_change))


# Join with data
data_min_max_hour <- left_join(data_min_max, hourly_depth_change, by = c("ID", "date_hour"))

# Keep relevant columns
data_min_max_hour <- select(data_min_max_hour, ID, date_hour, night_day, direction_x, direction_y, speed, direction, current_phase_x, current_phase_y, current_phase_p, hourly_depth_change)

# Remove doubles
data_min_max_hour <- data_min_max_hour %>%
  distinct()

# Remove duplicate since ocean current data someties goes from, for example, 04:05 - 05:00. When applying 'floor()' this results in doubly hourly timestamps
data_min_max_hour <- data_min_max_hour %>% group_by(ID, date_hour, night_day) %>% slice(-2)


# 6. Write csv ####
write.csv(data_min_max_hour, "./data/interim/data_depth_diff.csv")





